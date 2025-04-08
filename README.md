# Network Checker

The idea behind this app is to check connection to an address provided and return the result.
The result is one of the following:
* Connection is established
* Couldn't parse the given address
* Configured DNS couldn't resolve the address
* TCP handshake failed
* TLS handshake failed
* HTTP-level error, the status code is ...
* Connection failed, the problem is unknown


Required packages:
* libpcap-dev

