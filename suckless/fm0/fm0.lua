#!/usr/bin/env lua

local music_player = 'mpv --force-window=yes --x11-name FM0 -- '

local radio_stations = {
	['BBC Radio 1'] = {
		url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio1_mf_p',
		backup_url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio1_mf_q'
	},
	['BBC Radio 1Xtra'] = {
		url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio1xtra_mf_p',
		backup_url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio1xtra_mf_q'
	},
	['BBC Radio 2'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio2_mf_p'},
	['BBC Radio 3'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio3_mf_p'},
	['BBC Radio 4'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio4fm_mf_p'},
	['BBC Radio 4 Extra'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio4extra_mf_p'},
	['BBC Radio 5 Live'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio5live_mf_p'},
	['BBC Radio 5 Live Sports Extra'] = {
		url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_radio5extra_mf_p'
	},
	['BBC Radio 6 Music'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_6music_mf_p'},
	['BBC Asian Networks'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_asianet_mf_p'},
	['BBC Radio Scotland'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_scotlandfm_mf_p'},
	['BBC Radio nan Gàidheal'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_nangaidheal_mf_p'},
	['BBC Radio Wales'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_walesmw_mf_p'},
	['BBC Radio Cymru'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_cymru_mf_p'},
	['BBC Radio Ulster'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_ulster_mf_p'},
	['BBC Local Radio London'] = {url = 'http://bbcmedia.ic.llnwd.net/stream/bbcmedia_lrldn_mf_p'},
	['BBC World Service 24 Hour News'] = {
		url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws',
		backup_url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws_backup'
	},
	['BBC World Service English Radio'] = {
		url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk',
		backup_url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk_backup'
	},
	["WDVX – East Tennessee's Own"] = {url = 'http://wdvx.stream.egihosting.com:8000/?type=http'},
	['Country Radio Switzerland'] = {url = 'http://rs3.radiostreamer.com:9330'},
	['Radio FM4'] = {
		url = 'http://mp3stream1.apasf.apa.at',
		backup_url = 'mms://apasf.apa.at/fm4_live_worldwide'
	},
	['X 96.3 Nueva York'] = {
		url = 'http://54.235.35.44/univision-wxnyfmmp3-ibc1',
		backup_url = 'http://54.81.225.107/univision-wxnyfmmp3-ibc1'
	},
	['KISS 100 London'] = {url = 'http://listenapi.bauerradio.com/api8/sharpstream/?i=kissnational.aac'},
	['Lush 99.5'] = {url = 'http://mediacorp.rastream.com/995fm'},
	['Gold 905'] = {url = 'http://mediacorp.rastream.com/905fm'},
	['Ria 89.7'] = {url = 'http://mediacorp.rastream.com/897fm'},
	['91.3 Hot FM'] = {url = 'http://sph.rastream.com/913fm'},
	['Kiss 92.0'] = {url = 'http://sph.rastream.com/sph-kiss92'},
	['Symphony 92.4'] = {url = 'http://mediacorp.rastream.com/924fm'},
	['Yes 93.3'] = {url = 'http://mediacorp.rastream.com/933fm'},
	['93.8 Live'] = {url = 'http://mediacorp.rastream.com/938fm'},
	['Warna 94.2'] = {url = 'http://mediacorp.rastream.com/942fm'},
	['Class 95.0'] = {url = 'http://mediacorp.rastream.com/950fm'},
	['95.8 Capital'] = {url = 'http://mediacorp.rastream.com/958fm'},
	['XFM 96.3'] = {url = 'http://mediacorp.rastream.com/963fm'},
	['Love 97.2'] = {url = 'http://mediacorp.rastream.com/972fm'},
	['Power 98 FM'] = {url = 'http://provisioning.streamtheworld.c...POWER98aac.pls'},
	['98.7 FM'] = {url = 'http://mediacorp.rastream.com/987fm'},
	['UFM 100.3'] = {url = 'http://sph.rastream.com/1003fm'},
	['Plaza One'] = {url = 'http://radio.plaza.one/opus', backup_url = 'http://radio.plaza.one/ogg'}
}

local function play(id)
	if radio_stations[id] then
		local success, _, exit_code = os.execute(music_player .. radio_stations[id].url)
		if not success and exit_code ~= 4 then
			os.execute(music_player .. radio_stations[id].backup_url)
		end
		return true
	else
		return false
	end
end

local stations = 'quit\n'
for k in pairs(radio_stations) do
	stations = stations .. k .. '\n'
end

local station_file = io.popen('echo "' .. stations .. '" | rofi -dmenu -i -p "Play …"')
local station_name = string.sub(station_file:read('a'), 1, -2)

play(station_name)
