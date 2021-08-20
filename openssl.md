# openssl usage

### create self signed certificate

```
openssl req -newkey rsa:4096 -x509 -sha256 -days 3650 -nodes \
    -out example.crt -keyout example.key
```
This will also prompt for additional info such as organization name.

### get certificate fingerprint

`openssl x509 -noout -fingerprint -sha256 -inform pem -in example.crt`

### sources

https://linuxize.com/post/creating-a-self-signed-ssl-certificate/
