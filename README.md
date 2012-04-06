# Stableboy

Stableboy is a tool for provisioning machines to be used for testing.  In the case of basho_harness, stableboy returns harnesses by request.  Think Vagrant in Erlang, but not tied directly to Virtualbox.

Right now it is just a dumb script that returns a {IP, Port, User, Pass} tuple given a requested VM type {platform, version, architecture}.


