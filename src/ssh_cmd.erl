%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(ssh_cmd).

%% public api
-export([run/2,write/3]).

%% ssh_channel behavior
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2,
         code_change/3
        ]).

%% open a new session channel, set the environment, and exec a command
run (Conn,Command) ->
    {ok,Channel}=ssh_connection:session_channel(Conn,5000),
    {ok,Pid}=ssh_channel:start_link(Conn,Channel,?MODULE,[Conn,Channel]),
    ssh_channel:call(Pid,{exec,Command}).

%% upload a file
write (Conn,File,Text) ->
    {ok,Channel}=ssh_connection:session_channel(Conn,5000),
    {ok,Pid}=ssh_channel:start_link(Conn,Channel,?MODULE,[Conn,Channel]),
    ssh_sftp:write(Pid,File,list_to_binary(Text)).

%% state log for all the commands executed
-record(state, {conn,
                channel,
                from,
                exit_code,
                eof,
                stdout = <<>>,
                stderr = <<>>
               }).

%% initialize the state
init ([Conn,Channel]) ->
    {ok,#state{conn=Conn,channel=Channel}}.

%% execute a shell command on the harness machine
handle_call ({exec,Command},From,#state{conn=Conn,channel=Channel}=State) ->
    lager:info("... ~s",[Command]),
    ssh_connection:exec(Conn,Channel,Command ++ "\n",infinity),
    {noreply,State#state{from=From}}.

%% unimplemented feature
handle_cast (_,State) ->
    {noreply,State}.

%% the channel is ready for use
handle_msg ({ssh_channel_up,_Channel,_Conn},State) ->
    {ok,State}.

%% ssh messages: exit codes and piped standard output
handle_ssh_msg ({ssh_cm,_Pid,{data,_Channel,0,Data}},State) ->
    lager:debug("~s",[trim(Data)]),
    {ok,State#state{stdout = <<(State#state.stdout)/binary,Data/binary>>}};
handle_ssh_msg ({ssh_cm,_Pid,{data,_Channel,1,Data}},State) ->
    lager:debug("~s",[trim(Data)]),
    {ok,State#state{stderr = <<(State#state.stderr)/binary,Data/binary>>}};
handle_ssh_msg ({ssh_cm,_Pid,{exit_status,_Channel,Code}},State) ->
    {ok,reply(State#state{exit_code=Code})};
handle_ssh_msg ({ssh_cm,_Pid,{eof,_Channel}},State) ->
    {ok,reply(State#state{eof=true})}.

%% terminate
terminate (_Reason,_State) ->
    ok.

%% code_change
code_change (_OldVsn,State,_Extra) ->
    {ok,State}.

%% --------------------------------------------------------------------------
%% private api

reply (#state{exit_code=undefined}=State) ->
    State;
reply (#state{eof=undefined}=State) ->
    State;
reply (#state{exit_code=Code,from=From,stdout=Stdout,stderr=Stderr}=State) ->
    ssh_channel:reply(From,{result(Code),{Code,Stdout,Stderr}}),
    State.

%% return atom for exit code status
result (0) -> ok;
result (undefined) -> ok;
result (_) -> error.

%% remove extraneous whitespace from the end of a string
trim (S) -> hd(re:replace(S,"[ \t\r\n]*$","")).
