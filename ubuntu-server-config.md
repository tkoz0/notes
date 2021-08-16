# ubuntu server config

### networking

Starting in 18.04, the config files are in `/etc/netplan`. List servers by
adding these under the network interface, keeping indents consistent:
```yaml
nameservers:
    addresses:
    - 1.1.1.1
    - 1.0.0.1
    - 8.8.8.8
    - 8.8.4.4
```

The addresses can be specified as a list such as `[1.1.1.1,1.0.0.1]`. In order
to use DHCP for addresses only, add the following:
```yaml
dhcp4-overrides:
    use-dns: no
```

Add `optional: true` to each network interface that is unused so that the system
does not spend 2 minutes waiting for network to be configured.

Then run `sudo netplan apply`.
Source: https://linuxize.com/post/how-to-set-dns-nameservers-on-ubuntu-18-04/
