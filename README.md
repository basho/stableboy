**NOTE** This repo is not used anymore within Basho and is not supported
in any way.  It is open sourced because there are a few interesting
bits of code that might be useful to someone down the road.

# Stableboy

Stableboy is a tool for provisioning machines to be used for testing.
In the case of basho_harness, stableboy returns harnesses by request.
Think Vagrant-lite in Erlang, but not tied directly to Virtualbox.

## Config File

A `~/.stableboy` file is required for stableboy to run (or a different
file can be specified with the `--config_file` flag).  You can find a
sample config file in `priv/stableboy_sample_config`.

The needed contents of your `.stableboy` file will vary based on the
backend you chose.

## Common Usage

### List

To see what VM's are currently available, use the `list` command.

```text
$ ./stableboy list
{"ubuntu-1104-64",ubuntu,[11,0,4],64},
{"ubuntu-1104-64-2",ubuntu,[11,0,4],64},
{"ubuntu-1104-64-3",ubuntu,[11,0,4],64},
{"centos-57-64"},rhel,[5,7,0],64}
```

This tells you that there are two VM's available for provisioning.
The format of the return value is:

`{<name>, <platform>, <version>, <architecture>}`

See the `brand` command for adding this information to specific VMs
when using the sb_vbox or sb_smartos backends.

#### List by Environment (env file)

The file (full path) that describes the environment you want.  This is taken directly from
[basho_harness](https://github.com/basho/basho_harness) and an example
can be
[found here](https://github.com/basho/basho_harness/blob/master/envs/ubuntu-1104-64.config).

```text
$ ./stableboy list ubuntu-vm.config
{"ubuntu-1104-64",ubuntu,[11,0,4],64},
{"ubuntu-1104-64-2",ubuntu,[11,0,4],64},
{"ubuntu-1104-64-3",ubuntu,[11,0,4],64},
```

#### List multiple (by count)

You can specify a `--count` or `-n` on the command line to
request a number of the same VM's.  So for example, if you need two
Ubuntu 11.04 VM's, you can:

```text
$ ./stableboy --count 2 list ubuntu-vm.config
{"ubuntu-1104-64",ubuntu,[11,0,4],64},
{"ubuntu-1104-64-2",ubuntu,[11,0,4],64}
```

If the number of VM's requested isn't available, an `{error, Reason}`
tuple will be returned. You will not get a partial list of Vm's.

The count argument is only supported by the `sb_vbox` and `sb_smartos` backends.

### Get

To get access to one of these VM's, you can issue a `get` command.

The return value of `get` is the following format `{ok,
"<ip_address>", port, "user", "password"}` or `{error, "Reason"}`

Specify one or more (space separated) VMs by name as given by the `list`
command.

```text
$ ./stableboy get "ubuntu-1104-64"
{ok,"192.168.1.118",22,"root","root"}
```
### Brand

The `brand` command is used to associate the platform, version, and architecture
information with the VM:

```text
$ ./stableboy brand ubuntu-1104-64 ubuntu:11.0.4:64
```

This tells the backend to store the platform, version, and architecture
for the VM named "ubuntu-1104-64".

The `list` command returns information about the VMs available. For the sb_file backend,
the information comes from the configuration file. For sb_smartos and sb_vbox backends,
which do not require the VMs to be listed in the configuration file, some or all of the
information may need to be added to meta-data associated with the VM; this extra data
allows the `list` command for these VM manager backends to extract the information
dynamically.

In addition, you can optionally add a username and password for the VM.

```text
$ ./stableboy brand ubuntu-1104-64 ubuntu:11.0.4:64:root:toor
```

If you don't specify the username and password, then make sure you
set the global `vm_user` and `vm_pass` properties in the configuration
file.

### Snapshot

A snapshot of the VM can be taken by hand for use in the rollback command. In order to take
a snapshot, the VM must be powered off. Only the disk state is captured. Only one snapshot is
maintained per named VM. If a snapshot already exists, you must use the `--force` flag to allow
overwriting the existing snapshot. This command is not meant to be called by `basho_harness`.

```text
$ ./stableboy snapshot --force ubuntu-1104-64
```

### Rollback

Before a test is run, `basho_harness` may call `rollback` to force the VM into a powered off
and clean state. There is no force flag required, so be careful if you use this by hand.

```test
$ ./stableboy rollback ubuntu-1104-64
```

## Backends

Stableboy supports more than one way to get VM's. I'm referring to the
different ways as 'backends' for lack of a better term.

### File (sb_file)

The one that will get you up and running the fastest, but also the
least powerful, is the `sb_file` backend.  Currently, this is the
default backend.  The `priv/stableboy_sample_config` has a section
describing the settings needed for the `sb_file` backend.

#### What *Is* Supported

The sb_file backend doesn't do much, it can list out your available
VM's and get a single VM by name or by description (both described
above in the **Get** section.

#### What *Is Not* Supported

It does nothing smart with those VM's, so it will just do a simple
lookup for you and return you information from your `.stableboy`
config file. So:

1. No Snapshotting
1. No automatic rollback
1. No `--count` parameter usage

### Virtualbox (sb_vbox)

In order to use the virtual box backend, you will need to supply either
the `-i sb_vbox` command argument to stableboy or add the `{vm, sb_vbox}`
property to `priv/stableboy_sample_config`.

The `sb_vbox` backend dynamically queries the VirtualBox Manager to see
what VMs are installed for the `list` command. It also supports the `brand`
command to allow you to annotate a VM with Platform, Architecture, and
Username/Password information that is needed to support the `get` command.
Then, `get` will also dynamically query the installed VMs. Note that you
still have to install and register your own VMs with VirtualBox; but at
least you don't have to maintain a list of these VMs in the stableboy
configuration file. That means a cloned VM will show up automatically;
and you won't have to re-brand that cloned VM.

#### Install Guest Additions!

In addition, you will need to load the "guest additions" module to your
virtual box install. See [Guest Additions manual](http://www.virtualbox.org/manual/ch04.html)
for a detailed explanation. This addition is needed to allow sb_vbox to get
the IP address dynamically so that you can create/clone your virtual
machines any way you want.

#### Port Rules

If you setup a port forwarding rule (e.g. for use with NAT'd networks),
make sure you name your rule with the string `ssh` somewhere in it.
sb_vbox looks for a rule with that string and extracts the forwarded
port (if it finds one) as the returned public port for accessing the
VM. For example, the most common documentation examples show the
creation of a port forward rule called "guestssh" so that works great.
With not port forwarding rules, the return port always defaults to 22.
There is currently no way to know if ssh is configured to work on a
different port. So use 22 or use a port forwarding rule.

#### Network Types

Be aware that the IP address returned by `sb_vbox` is the one associated
with the first NIC (adapter 0), so make sure that if you add multiple NICs,
the first one is the one you want to use for stableboy's public interface.

You can create any type of network: bridged, NAT'd, and internal. Of course,
internal is not useful if that's your first NIC. Don't do that. Make your
first NIC either bridged or NAT'd. If you use NAT, make sure you have a
port forwarding rule for ssh as described above in Port Rules, otherwise
your VMs are unreachable.

For a description of networking types supported in VirtualBox, see
[the manual](https://www.virtualbox.org/manual/ch06.html).

#### What *Is Not* Supported

Cloning via stableboy is not supported, so if `get` asks for more VMs than
are available (via the `count` argument), then it will come up short.

If you don't supply a Username and Password when you `brand` the VM, it
will not return the default ones from the configuration file. That's a
bug that needs to be fixed.

### SMartOS (sb_smartos)

The SmartOS (KVM) backend supports the same set of commands as the
VirtualBox backed, which means you can use the exact same syntax for
the `brand` command to annotate your VM without needing to know how
it works.

I'm not sure if it handles port forwarding rules, so for now it's
best to ensure that port 22 is the public ssh port. The same applies
with automatic cloning - not supported yet. Same rule about supplying
Username/Password as `sb_vbox` as well.

**NOTE** Before you can connect to your global zone, you need to add the
following line to your `/etc/ssh/sshd_config` file:

`Ciphers aes128-ctr,aes192-ctr,aes256-ctr,arcfour128,arcfour256,arcfour,aes128-cbc,3des-cbc`

Then restart sshd with

`$ svcadm refresh ssh`



## Future / TODO

1. Return default "global" Username/Password if branded pair isn't found.
2. Maybe support cloning.
3. Command for deletion of VM clones

## Optional Flags

### --vm or -i

Currently this defaults to `sb_file`. Other backends supported are
SmartOS (via `-i sb_smartos`) and Virtual Box (via `-i sb_vbox`).

### --debug or -d

Print a **lot** of debug output.

### --config_file or -f

Use an alternate stableboy config file besides ~/.stableboy
