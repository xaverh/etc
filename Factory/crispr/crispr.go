package main

import (
	"net"
)

type dnsServer struct {
	name      string
	addresses [4]string
}

var networkDevices, _ = net.Interfaces()

var dnsServers = []dnsServer{
	{"🔙 reset", {"", "", "", ""}},
	{"Google", {"8.8.8.8", "8.8.4.4", "2001:4860:4860::8888", "2001:4860:4860::8844"}},
	{"1⁴", {"1.1.1.1", "1.0.0.1", "2606:4700:4700::1111", "2606:4700:4700::1001"}},
	{"SmartDNSProxy", {"54.93.173.153", "46.166.189.68", "81.17.17.170", "35.178.60.174"}},
	{"Fritz!Box", {"192.168.178.1", "", "", ""}},
	{"LRZ", {"10.156.33.53", "129.187.5.1", "2001:4ca0::53:1", "2001:4ca0::53:2"}},
	{"Mullvad", {"193.138.218.74", "", "", ""}},
	{"DNS.WATCH", {"84.200.69.80", "84.200.70.40", "2001:1608:10:25::1c04:b12f", "2001:1608:10:25::9249:d69b"}},
}

func main() {
	return
}