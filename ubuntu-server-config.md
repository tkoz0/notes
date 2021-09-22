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

### wireguard basics

Generating a keypair: `wg genkey | tee privatekey | wg pubkey > publickey`

Put the private key here: `cp privatekey /etc/wireguard/wg1.key`

Put the wireguard config file at: `/etc/wireguard/wg1.conf`

Enable the `wg1` network interface on startup.
```bash
sudo systemctl enable wg-quick@wg1
sudo systemctl start wg-quick@wg1
```

### zfs

The pool may not mount automatically if you create it with /dev/sdX devices. To
fix this, the following command will help:
`zpool import -d /dev/disk/by-id -aN`

To create a zpool: `zpool create <name> [type] drives...`. It creates a mount
point at `/<name>`. Type can be `raidz`, `raidz2`, or `mirror`.

### zram

Setup (will be available on next reboot)
```
sudo apt install util-linux zram-config
cat /proc/swaps
```

Edit zram size in `/usr/bin/init-zram-swapping`. Edit the
`mem=$(((totalmem / 2 / ${NRDEVICES}) * 1024 ))` line. Default is half of the
RAM.

Possible changes
- Remove the `/ 2` to use the same size as the RAM
- Change `1024` to `1536` to use 75% of the RAM

Edit the zram algorithm by finding the
`echo $mem > /sys/block/zram${DEVNUMBER}/disksize` line. Below it, put a line
like `echo zstd > /sys/block/zram${DEVNUMBER}/comp_algorithm`. The algorithm
could also be `lz4` or `lzo` or some other options.
