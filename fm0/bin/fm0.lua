#!/usr/bin/env lua

os.execute('herbstclient substitute 🏷️ my_📻 close 🏷️')

local radio_stations = {
	['🇬🇧 🔥\tBBC Radio 1'] = {
		-- X
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_one.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_one.m3u8'
	},
	['🇬🇧 👨🏿‍🎤\tBBC Radio 1Xtra'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_1xtra.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_1xtra.m3u8'
	},
	['🇬🇧 🎸\tBBC Radio 2'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_two.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_two.m3u8'
	},
	['🇬🇧 🎻\tBBC Radio 3'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_three.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_three.m3u8'
	},
	['🇬🇧 🎙️\tBBC Radio 4'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_fourfm.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_fourfm.m3u8'
	},
	['🇬🇧 🎙️\tBBC Radio 4 Extra'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_four_extra.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_four_extra.m3u8'
	},
	['🇬🇧 ⚽\tBBC Radio 5 Live'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_five_live.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_five_live.m3u8'
	},
	['🇬🇧 🏏\tBBC Radio 5 Live Sports Extra'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_five_live_sports_extra.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_five_live_sports_extra.m3u8'
	},
	['🇬🇧 🥑\tBBC Radio 6 Music'] = {
		-- X
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_6music.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_6music.m3u8'
	},
	['🇬🇧 🛕\tBBC Asian Networks'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_asian_network.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_asian_network.m3u8'
	},
	['🇬🇧 🏴󠁧󠁢󠁳󠁣󠁴󠁿\tBBC Radio Scotland'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_scotland_fm.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_scotland_fm.m3u8'
	},
	['🇬🇧 🏴󠁧󠁢󠁷󠁬󠁳󠁿\tBBC Radio Wales'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_radio_wales_fm.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_radio_wales_fm.m3u8'
	},
	['🇬🇧 🏴󠁧󠁢󠁥󠁮󠁧󠁿\tBBC Local Radio London'] = {
		url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/uk/sbr_high/ak/bbc_london.m3u8',
		backup_url = 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/hls/nonuk/sbr_low/ak/bbc_london.m3u8'
	},
	['🇬🇧 🌍\tBBC World Service'] = {
		url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws',
		backup_url = 'http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws_backup'
	},
	['🇺🇸 🪕\tWDVX – East Tennessee’s Own'] = {
		url = 'https://wdvx.streamguys1.com/live.xspf',
		backup_url = 'https://wdvx.streamguys1.com/live.m3u'
	},
	['🇨🇭 🪕\tCountry Radio Switzerland'] = {
		url = 'http://rs4.radiostreamer.com:8000', -- 128 k
		backup_url = 'https://www.countryradio.ch/crs-64.pls' -- 64 k
	},
	['🇦🇹 🥑\tRadio FM4'] = {
		url = 'https://fm4shoutcast.sf.apa.at/listen.pls',
		backup_url = 'http://mp3stream1.apasf.apa.at'
	},
	['🇦🇹 🎸\tÖ3'] = {
		url = 'https://oe3shoutcast.sf.apa.at/'
	},
	['🇺🇸 💃🏽\tWXNY – X 96.3 Nueva York'] = {
		url = 'https://prod-107-23-202-222.wostreaming.net/univision-wxnyfmaac-imc1',
		backup_url = 'http://in.icy2.abacast.com/univision-wxnyfmaac-im.m3u'
	},
	['🇬🇧 🔥\tKISS 100 London'] = {
		url = 'http://listenapi.bauerradio.com/api8/sharpstream/?i=kissnational.aac',
		backup_url = 'http://tx.whatson.com/http_live_bauer.php?i=kissnational.aac'
	},
	['🇸🇬 🎸\tClass 95'] = {
		url = 'http://playerservices.streamtheworld.com/api/livestream-redirect/CLASS95_PREM.m3u8',
		backup_url = 'http://mediacorp.rastream.com/950fm'
	},
	['🇺🇸 🦄\tNightwave Plaza'] = {url = 'http://radio.plaza.one/opus', backup_url = 'http://radio.plaza.one/ogg'},
	['🇩🇪 🥑\tDeutschlandfunk Nova'] = {url = 'http://st03.dlf.de/dlf/03/104/ogg/stream.ogg'},
	['🇩🇪 🎙️\tDeutschlandfunk'] = {
		url = 'http://st01.dlf.de/dlf/01/104/ogg/stream.ogg',
		backup_url = 'https://www.deutschlandradio.de/streaming/dlf_hq_ogg.m3u'
	},
	['🇪🇸 🔥\tLos 40'] = {
		url = 'https://20873.live.streamtheworld.com/LOS40AAC.aac'
	},
	['🇪🇸 💃🏽\tLos 40 Latin'] = {
		-- url = 'https://20853.live.streamtheworld.com/LOS40_03AAC.aac',
		url = 'https://17873.live.streamtheworld.com/LOS40AAC.aac',
		backup_url = 'https://20103.live.streamtheworld.com/LOS40_03AAC.aac'
	},
	['🇨🇴 💃🏽\tLos 40 Medellín'] = {
		url = 'https://14073.live.streamtheworld.com/LOS40COL_MEDAAC.aac'
	},
	['🇺🇸 🤘🏻\tKLOS — 95.5 Southern California’s Classic Rock'] = {
		url = 'http://19273.live.streamtheworld.com/KLOSFMAAC.aac'
	},
	['🇨🇭 🍺\tRadio Melody'] = {
		url = 'https://fm1melody.ice.infomaniak.ch/fm1melody-128.mp3'
	},
	['🇩🇪 🍺\tHeimatradio Melodie'] = {
		url = 'http://212.77.178.166:8020/',
		backup_url = 'http://212.77.178.166/listen.pls'
	},
	['🇩🇪 ✌🏻\tBayern 1 Oberbayern'] = {
		url = 'http://streams.br.de/bayern1obb_2.m3u',
		backup_url = 'http://streams.br.de/bayern1obb_1.m3u'
	},
	['🇩🇪 🎙️\tBayern 2 Süd'] = {
		url = 'http://streams.br.de/bayern2sued_2.m3u',
		backup_url = 'http://streams.br.de/bayern2sued_1.m3u'
	},
	['🇩🇪 🎸\tBayern 3'] = {
		url = 'http://streams.br.de/bayern3_2.m3u',
		backup_url = 'http://streams.br.de/bayern3_1.m3u'
	},
	['🇩🇪 🎻\tBR Klassik'] = {
		url = 'http://streams.br.de/br-klassik_2.m3u',
		backup_url = 'http://streams.br.de/br-klassik_1.m3u'
	},
	['🇩🇪 📰\tB5 aktuell'] = {
		url = 'http://streams.br.de/b5aktuell_2.m3u',
		backup_url = 'http://streams.br.de/b5aktuell_1.m3u'
	},
	['🇩🇪 ⚽\tB5 plus'] = {
		url = 'http://streams.br.de/b5plus_2.m3u',
		backup_url = 'http://streams.br.de/b5plus_1.m3u'
	},
	['🇩🇪 🍺\tBayern plus'] = {
		url = 'http://streams.br.de/bayernplus_2.m3u',
		backup_url = 'http://streams.br.de/bayernplus_1.m3u'
	},
	['🇩🇪 🥑\tPuls'] = {
		url = 'http://streams.br.de/puls_2.m3u',
		backup_url = 'http://streams.br.de/puls_1.m3u'
	},
	['🇩🇪 🏔️\tBR Heimat'] = {
		url = 'http://streams.br.de/brheimat_2.m3u',
		backup_url = 'http://streams.br.de/brheimat_1.m3u'
	}
}

local function play(id)
	if radio_stations[id] then
		local success, _, exit_code = os.execute('mpv --mute=no --x11-name=FM0 --force-window=yes ' .. radio_stations[id].url)
		if not success and exit_code ~= 4 then
			os.execute('mpv --mute=no --x11-name=FM0 --force-window=yes ' .. radio_stations[id].backup_url)
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

local station_file = io.popen('echo "' .. stations .. '" | rofi -dmenu -i -no-custom -p "Play …"')
local station_name = string.sub(station_file:read('a'), 1, -2)

play(station_name)
