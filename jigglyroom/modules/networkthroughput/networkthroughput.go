package networkthroughput

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"

	"../../config"
	"../../lemonbar"
)

// NetworkThroughput gives information about the up- and downstream of given network devices
type NetworkThroughput struct {
	NetDevs []string
	Output  chan string
}

func fixed(pre string, rate int) string {
	if rate < 0 {
		return pre + " err"
	}

	var suf = config.KbpsSign
	if rate > 1000 {
		var result = float64(rate)

		switch {
		case rate >= (1000000000): // GB/s
			result /= 1000000000.0
			suf = config.GbpsSign
		case rate >= (1000000): // MB/s
			result /= 1000000.0
			suf = config.MbpsSign
		case rate >= (1000): // kB/s
			result /= 1000.0
			suf = config.KbpsSign
		default:
			suf = config.BpsSign
		}

		return fmt.Sprintf("%s%.1f %s", pre, result, suf)

	}
	return fmt.Sprintf("%s%d %s", pre, rate, suf)
}

func (networkThroughput NetworkThroughput) PrintToChannel() {
	rxOld := 0
	txOld := 0
	rate := 2
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			networkThroughput.Output <- config.NetReceivedSign + " err " + config.NetTransmittedSign + " err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			dev = strings.TrimSuffix(dev, ":")
			for _, val := range networkThroughput.NetDevs {
				if val == dev {
					rxNow += rx
					txNow += tx
				}
			}
		}
		file.Close()
		networkThroughput.Output <- fmt.Sprintf("%s%s%s", fixed(config.NetReceivedSign, (rxNow-rxOld)/rate), lemonbar.SeparatorModules, fixed(config.NetTransmittedSign, (txNow-txOld)/rate))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
	}
}

func (networkThroughput NetworkThroughput) GetOutputChannel() chan string {
	return networkThroughput.Output
}
