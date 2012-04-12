# Stableboy

Stableboy is a tool for provisioning machines to be used for testing.  In the case of basho_harness, stableboy returns harnesses by request.  Think Vagrant in Erlang, but not tied directly to Virtualbox.

## Config File
A `~/.stableboy` file is required for stableboy to run (or a different file can be specified with the `--config_file` flag).  You can find a sample config file in `priv/stableboy_sample_config`.

## Common Usage

### List
To see what VM's are currently available, use the `list` command.

```text
$ ./stableboy list
{"ubuntu-1104-64",ubuntu,[11,0,4],64}
{"centos-57-64"},rhel,[5,7,0],64}
```

This tells you that there are two VM's available for provisioning.  The format of the return value is:

`{<name>, <platform>, <version>, <architecture>}`

### Get

To get access to one of these VM's, you can issue a `get` command.

The return value of `get` is the following format
`{ok, "<ip_address>", port, "user", "password"}`
or
`{error, "Reason"}`

The `get` command has a few options in its use.

The first can be a file (full path) that describes the environment you want.   This is taken directly from [basho_harness](https://github.com/basho/basho_harness) and an example can be [found here](https://github.com/basho/basho_harness/blob/master/envs/ubuntu-1104-64.config).

```text
$ ./stableboy get ubuntu-vm.config
{ok,"192.168.1.118",22,"root","root"}
```

Second, you can specify a particular VM by name as given by the `list` command.

```text
$ ./stableboy list
{"ubuntu-1104-64",ubuntu,[11,0,4],64}
{"centos-57-64"},rhel,[5,7,0],64}
$ ./stableboy get "ubuntu-1104-64"
{ok,"192.168.1.118",22,"root","root"}
```

Lastly, you can specify a `--count` or `-n` on the command line to request a number of the same VM's.  So for example, if you need four Ubuntu 11.04 VM's, you can:

```text
$ ./stableboy --count 4 get ubuntu-vm.config
{ok,"192.168.1.118",22,"root","root"}
{ok,"192.168.1.119",22,"root","root"}
{ok,"192.168.1.120",22,"root","root"}
{ok,"192.168.1.121",22,"root","root"}
```

If the number of VM's requested isn't available, an `{error, Reason}` tuple will be returned. You will not get a partial list of Vm's.


## Optional Flags

### --vm or -i
Currently this defaults to `vbox`.  This will in the future be used to support different backends such as SmartOS (KVM)

### --debug or -d
Print a **lot** of debug output.

### --config_file or -f
Use an alternate stableboy config file besides ~/.stableboy




