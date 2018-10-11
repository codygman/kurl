# Kurl
A twitch vod downloader in haskell


# Installation

 ## edit kurl.conf fields
  - kurlConfClientId: You need get `Client-ID` from [twitch dev console](https://glass.twitch.tv/console/apps)
  - kurlConfUserLoginName: Your default user id. this is not yet implemented

 ## copy kurl.conf to $HOME/.config/kurl/kurl.conf


# Usage

## Command table
|---------+------------------------------+------------------------------------------------+------------------------------|
| command | argument type                | explanation                                    | notes                        |
|---------+------------------------------+------------------------------------------------+------------------------------|
| list    | twitch login name            | show the live streams of the followed channels |                              |
| m3u     | twitch login name or vod url | show the m3u8 file url                         | can be used by video players |
| enc     | twitch login name or vod url | show encoding command                          | possible to set range        |
| chat    | twitch vod url               | download chat log                              | possible to set range        |
|---------+------------------------------+------------------------------------------------+------------------------------|

## Command example
|---------+--------------------------------------------------------------------+
| command | example                                                            | 
|---------+--------------------------------------------------------------------+
| list    | `kurl list ninja`                                                  | 
| m3u     | `kurl m3u ninja`                                                   | 
| m3u     | `kurl m3u https://www.twitch.tv/videos/123456789`                  | 
| enc     | `kurl enc ninja`                                                   | 
| enc     | `kurl enc https://www.twitch.tv/videos/123456789 s=1h30m e=2h3m1s` | 
| chat    | `kurl chat https://www.twitch.tv/videos/123456789 s=1h`            | 
|---------+--------------------------------------------------------------------+

## Range format
|----------------------------------------------------+----------------------------+------------|
| range value type                                   | range value foramt         | exmpale    |
|----------------------------------------------------+----------------------------+------------|
| hour                                               | 0h ~ ...h                  | `99h`      |
| minutes                                            | 0m ~ ...m                  | `130m`     |
| seconds                                            | 0s ~ ...s                  | `420s`     |
| hour-minutes                                       | 0h ~...h 0m ~ 59m          | `12h30m`   |
| minutes-seconds                                    | 0m ~...m 0s ~ 59s          | `90m10s`   |
| hour-seconds                                       | 0h ~...h 0s ~ 59s          | `24h10s`   |
| hour-minutes-seconds                               | 0h ~...h 0m ~ 59m 0s ~ 59s | `48h10m10s`|
|----------------------------------------------------+----------------------------+------------|
-  note on range: Either start or the end of the range can be omitted .
          if start omitted, then start becomes 0s.
          if end omitted, then end becomes length of the stream.
