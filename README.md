This is a redacted version of the repo I originally wrote this project in,
because I wasn't planning to publish it at the time, so I included my API key
and stuff in there.

I used `git filter-branch` to wipe out the file `src/Secret.hs`, which looks
like this:

```
{-# LANGUAGE OverloadedStrings #-}
module Secret where

import Web.Authenticate.OAuth (OAuth(..))
import Web.Twitter.Conduit (twitterOAuth)

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "REDACTED"
    , oauthConsumerSecret = "REDACTED"
    }
```

Forgive me for my youthful indiscretions, we were all young once.
