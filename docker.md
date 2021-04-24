# docker setup/use

### installing

https://docs.docker.com/engine/install/ubuntu/

Commands to install as recommended
```bash
sudo apt remove docker docker-engine docker.io containerd runc
sudo apt update
sudo apt install apt-transport-https ca-certificates curl gnupg lsb-release
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
sudo docker run hello-world
```
Uninstallation
```bash
sudo apt-get purge docker-ce docker-ce-cli containerd.io
sudo rm -rf /var/lib/docker
sudo rm -rf /var/lib/containerd
```

### docker as non-root

Create a group called `docker` and add users to it.
```bash
sudo groupadd docker
sudo usermod -aG docker $USER
```
After logging out and in again (re-evaluates group membership), verify by
running `docker run hello-world`.

### docker on startup

Enabled by default on Debian/Ubuntu. Otherwise, run these:
```bash
sudo systemctl enable docker.service
sudo systemctl enable containerd.service
```

### specifying dns

Edit `/etc/docker/daemon.json` to add a JSON key with a DNS server list:
```json
{
    "dns": ["1.1.1.1", "8.8.8.8", "8.8.4.4"]
}
```
Then run `sudo service docker restart`. If things are working properly, then
this should work: `docker pull hello-world`.
