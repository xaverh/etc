package cputemperature

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"time"
)

// CPUTemperature returns the current temperature of the specified heat sink
type CPUTemperature struct {
	ThermalZone int
	Output      chan string
}

func (cpuTemperature CPUTemperature) PrintToChannel() {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + strconv.Itoa(cpuTemperature.ThermalZone) + "/temp")
		if err != nil {
			cpuTemperature.Output <- "temp unknown"
		}
		cpuTemperature.Output <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		time.Sleep(time.Duration(7 * time.Second))
	}
}

func (cpuTemperature CPUTemperature) GetOutputChannel() chan string {
	return cpuTemperature.Output
}
