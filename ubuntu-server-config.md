# ubuntu server config

### networking

Starting in 18.04, the config files are in `/etc/netplan`. List servers by
adding these under the network interface, keeping indents consistent:
```yaml
nameservers:
    addresses:
    - 1.1.1.1
    - 8.8.8.8
    - 8.8.4.4
```
Then run `sudo netplan apply`.
Source: https://linuxize.com/post/how-to-set-dns-nameservers-on-ubuntu-18-04/
