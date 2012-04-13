# Stableboy

Stableboy is a tool for provisioning machines to be used for testing.  In the case of basho_harness, stableboy returns harnesses by request.  Think Vagrant in Erlang, but not tied directly to Virtualbox.

## Config File
A `~/.stableboy` file is required for stableboy to run (or a different file can be specified with the `--config_file` flag).  You can find a sample config file in `priv/stableboy_sample_config`.

The needed contents of your `.stableboy` file will vary based on the backend you chose.

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

#### Get by Environment (env file)

The first can be a file (full path) that describes the environment you want.   This is taken directly from [basho_harness](https://github.com/basho/basho_harness) and an example can be [found here](https://github.com/basho/basho_harness/blob/master/envs/ubuntu-1104-64.config).

```text
$ ./stableboy get ubuntu-vm.config
{ok,"192.168.1.118",22,"root","root"}
```

#### Get by Name

Second, you can specify a particular VM by name as given by the `list` command.

```text
$ ./stableboy list
{"ubuntu-1104-64",ubuntu,[11,0,4],64}
{"centos-57-64"},rhel,[5,7,0],64}
$ ./stableboy get "ubuntu-1104-64"
{ok,"192.168.1.118",22,"root","root"}
```

#### Multi-Get (count)

Lastly, you can specify a `--count` or `-n` on the command line to request a number of the same VM's.  So for example, if you need four Ubuntu 11.04 VM's, you can:

```text
$ ./stableboy --count 4 get ubuntu-vm.config
{ok,"192.168.1.118",22,"root","root"}
{ok,"192.168.1.119",22,"root","root"}
{ok,"192.168.1.120",22,"root","root"}
{ok,"192.168.1.121",22,"root","root"}
```

If the number of VM's requested isn't available, an `{error, Reason}` tuple will be returned. You will not get a partial list of Vm's.

Multi-get is only supported by the `vbox` backend currently.


## Backends

Stableboy supports more than one way to get VM's. I'm referring to the different ways as 'backends' for lack of a better term.

### File (file)

The one that will get you up and running the fastest, but also the least powerful, is the `file` backend.  Currently, this is the default backend.   The `priv/stableboy_sample_config` has a section describing the settings needed for the `file` backend.

#### What *Is* Supported

The file backend doesn't do much, it can list out your available VM's and get a single VM by name or by description (both described above in the **Get** section.

#### What *Is Not* Supported

It does nothing smart with those VM's, so it will just do a simple lookup for you and return you information from your `.stableboy` config file. So:

1. No Snapshotting
1. No automatic rollback
1. No `--count` parameter usage


### Virtualbox (vbox)

At least initially, this will be the most common backend to use and will have the biggest feature set.  See the `priv/stableboy_sample_config` for the settings that are needed for the `vbox` backend.

#### What *Is* Supported

You can `list` your VM's, but like the `file` backend, this will only list the VM's in your `vms` list in your `.stableboy` config file as well as any dynamic clones created when using the `--count` option.

`get` is also supported, and like the `file` backend, it follows the pattern as described in the **Get** section above.  Unlike the `file` backend though, the `vbox` backend does support the use of `--count`.  If requesting a `--count 5` of a certain type of VM, it will clone that VM into `count - 1` clones and return the information on the original VM and the clones.

#### Future / TODO

1. Automatic rollback to last snapshot before getting a VM
1. command for deletion of VM clones

#### Gotchas or Known Issues

You **cannot** use Bridged Networking for your virtualbox VM's.  There is no good way to retrieve the IP address of VM's in that case.   If you need Bridged Networking, try using the `file` backend and specify your IP addresses manually.  For a description of networking types supported in VirtualBox, see [the manual](https://www.virtualbox.org/manual/ch06.html).


## Optional Flags

### --vm or -i
Currently this defaults to `file`.  This will in the future be used to support different backends such as SmartOS (KVM)

### --debug or -d
Print a **lot** of debug output.

### --config_file or -f
Use an alternate stableboy config file besides ~/.stableboy




