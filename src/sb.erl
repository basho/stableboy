-module(sb).

-export([load_config/1, set_config/2, get_config/1, get_config/2]).

%%
%% Application Config
%%

load_config(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            [set_config(Key, Value) || {Key, Value} <- Terms],
            ok;
        {error, Reason} ->
            lager:error("Failed to parse config file: ~p with error: ~p", [File, Reason]),
            {error, Reason}
    end.


set_config(Key, Value) ->
    ok = application:set_env(stableboy, Key, Value).


get_config(Key) ->
    case application:get_env(stableboy, Key) of
        {ok, Value} ->
            Value;
        undefined  ->
            lager:error("Missing configuration key: ~p", [Key])
    end.

get_config(Key, Default) ->
    case application:get_env(stableboy, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

