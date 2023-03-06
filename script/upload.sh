#!/usr/bin/env bash
curl -i  \
  -H "DD-API-KEY: ${DD_API_KEY}" \
  -H "DD-EVP-ORIGIN: test-origin"\
  -H "DD-EVP-ORIGIN-VERSION: test-origin-version"\
  -F "main.jfr=@${1:?file missing}" \
  -F "event=@-;type=application/json" \
  https://intake.profile.datadoghq.com/api/v2/profile <<END
{
 "attachments":[ "main.jfr" ],
 "tags_profiler":"service:LiSP,version:${2}",
 "start":"$(date -u --iso-8601=seconds | sed 's/\+.*/Z/')",
 "end":"$(date -d '+1 sec' -u --iso-8601=seconds | sed 's/\+.*/Z/')",
 "family":"java",
 "version":"4"
}
END
