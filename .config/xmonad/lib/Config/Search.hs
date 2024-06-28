module Config.Search where

import XMonad (X)
import XMonad.Actions.Search (SearchEngine, alpha, aur, cratesIo, hoogle, promptSearch, rustStd, searchEngine, stackage)

import Config.Prompt (myXPConfig)

type SearchList =
        [ ( String -- Keybind predicate
          , SearchEngine -- Search engine to use
          )
        ]

toSearchKeybinds ::
        Char -> -- Group prefix
        SearchList -> -- Search list to construct keybinds for
        [(String, X ())] -- Resulting group of keybinds
toSearchKeybinds groupPrefix list =
        [("M-f " ++ [groupPrefix] ++ " " ++ prefix, promptSearch myXPConfig engine) | (prefix, engine) <- list]

mySearchKebinds :: [(String, X ())]
mySearchKebinds =
        concatMap (uncurry toSearchKeybinds) searchGroups
                -- Standalone keybinds
                ++ standalone
    where
        searchGroups :: [(Char, SearchList)]
        searchGroups =
                [ ('h', haskellSearchList)
                , ('r', rustSearchList)
                , ('a', archSearchList)
                , ('g', gentooSearchList)
                ]
        standalone :: [(String, X ())]
        standalone =
                [ ("M-f w", promptSearch myXPConfig alpha)
                , ("M-f b", promptSearch myXPConfig braveSearch)
                , ("M-f s", promptSearch myXPConfig searx)
                ]

haskellSearchList :: SearchList
haskellSearchList =
        [ ("h", hoogle)
        , ("w", haskellWiki)
        , ("s", stackage)
        ]
    where
        haskellWiki = searchEngine "Haskell Wiki" "https://wiki.haskell.org/index.php?title=Special:Search&search="

rustSearchList :: SearchList
rustSearchList =
        [ ("s", rustStd)
        , ("c", cratesIo)
        ]

archSearchList :: SearchList
archSearchList =
        [ ("w", archWiki)
        , ("u", aur)
        , ("p", archRepo)
        ]
    where
        archWiki = searchEngine "Arch Linux Wiki" "https://wiki.archlinux.org/index.php?search="
        archRepo = searchEngine "Arch Linux Repository" "https://archlinux.org/packages/?sort=&q="

gentooSearchList :: SearchList
gentooSearchList =
        [ ("w", gentooWiki)
        , ("p", gentooPackages)
        ]
    where
        gentooWiki = searchEngine "Gentoo Linux Wiki" "https://wiki.gentoo.org/index.php?title=Special:Search&search="
        gentooPackages = searchEngine "Gentoo Linux Packages" "https://packages.gentoo.org/packages/search?q="

braveSearch :: SearchEngine
braveSearch = searchEngine "Brave Search" "https://search.brave.com/search?q="

searx :: SearchEngine
searx = searchEngine "SearXNG" "https://searx.be/?preferences=eJx1WEuP5DYO_jVbl8IUspsFghzqFCDXLLC5G7RE2xpLokaSq8r960P6UZbaPYc2Sh8piuJbrSBjT9FguvfoMYK9WPD9BD3ewfKCFFi8o7_AlEmRCxYz3nui3uLFOOZrQqTXfP87TnhxmAfS9__99f-_Lwk6TAhRDfdfLnlAh_dkZP8lYppsTg35xuOzydDe_wSb8KLJNEwk-8B4T8p8G6b2xjglBfFbyjOrYqk3ijQ-vmmI42UV2aw00fGi0GeMDVjTe8e_V81AP8Ar1M2m0nrgjwnj3BjfZJN5_8JpfGe8ySxTRbJ2BdddorFaLTazIIsqb4IGyiPO6a6xA77bRZsEreXj0PfGs3V_76FvmkTKgL061Ab-9Z8_wHtIV5FsHtg0nbGYBA7j1ZkYKZYYX_PK32vKFEtmD4q8hqYxWZYxG8W_xTMLOY1Ns7mWl62xRv6a5mE0knC0oa04sjZ9f8ingJ7dkbAQuuh_U0rdNBaSVAgRO4zIht7UYROmxCBfXRnBV-ypH4Y9Xxw7Y_i0vK7HFccKyAHDkHxF0hStwUp7GPxAXQlpxA-Oh8ZNyahl_TDgMxuqEK11f2XfieMN-VTuf5rRaMhQYqs3HATm5C8jmb08V6r8-1XI73Qkow-rdtaoMZYMEfGaqMtPiHjVJnJsSZStZuyi8aOB0rFrCl6DhVnCIh2yS4ojNnQqfOQ4fKPEICeRL43bB9K6tNMAbQT5bDoM-Np-GddPpe7GQ3ECB1iEOF-pu_IRPedzeYxQMUfwybImpb24doAK4LdDHH0MpqI_XWtLwM8ARZzqOWEZjRK4EQMVlgmgRuhNytsZwQRO0WMLh--W6MUxb_AckAdpFfH1tipkA1c0jsBdgal1osGRHOseNydTWydhMLAr91Z4BaQcXuWzSc00zpQpDTSKOffbc101moOK8zMXRslymaQGmMrzSnRTffKJYyoNZUbSlKe2tPobeacbGDs7krQq2B7GIRXrp2nnKsEsvsBrjtTPqdgSjaf8_DFRbS4BE01RndGAakmJn8CHKwSWymfy_Jn7QbN0vhIl20VwYMNQqayNyh_k68rw66-_vQq_4IcHVzLEqZ17dGmPEsT4ycxLBeamOkodfWJbegBUpa-sz6EbJ84mrNL2gYkTBKvwwjHW8btCJ3ErvGfBIYAzXeUqjh-9JGWxVezpuPCWXFzC3ssL-rqVLklljZ9eV9n7bnwv8yjd13L8KHDhCEXhDmK4uiP6_vP6fL8FrQyxIKcLtya3kxpxrzBqwG4kyVlib8W1Z0mDnOsWlTJkkw4uTSwkXnn-2QQhN01pIG8GPkX-eqoEvdHzFQraSe2CVl2ywJ8o8RYPDdDR992cbyxzqnv2DR5YN8-F3qyrmdLhE15ncfZyyZ6nMGiPxXF9qW4txn6nEemIoAuLrV3vUxBJHzwZYsOri27YyTAbzoWQLMQyvAYQD61C1v64KbC1SN0euhnPAg1NpeQC223x3XDJPnaNPCJAKiq1RedmLvTOTTKnLNWqHihXjilx1foJjfth_hlNJK-94TN5a-li22QKjT6-baRCSc6LR5VhltqU8RZ3QzngEVaT_4meb_LAvYbb0Bcc-j0kmJeyNOnDhj649zE8MuE-vcg0ADxVySSXMFeFfqdxN-axDLYJsKbzdIKFbwTi4oaYefzbZ8CgpUYf-wK_CqTrbVQj7xJM5di5DCC3RffjCsFEHo1aKIzKs5lisxivZQjblQhz2KvfjyePtKXRF6CO8RU6pcMKnyI_AkfnteWyk8qpkC2oTT65JFLOksk8XhIWWck9mKtw7SKudWok7qydpedevdM4tZPP054_YivPE-H6Nvoi_9IUME7p7eCE7HB--3yVq0mm_fCpY-cqRWcYiOq2yUBtQOkfnwEJliX-d0HCJInEMXQ72boknixeEmFi69edayctMXGEhpsdV2JOzvdQreuOoNEf2nHN5k5djdXyQgjGUj64PDzEruV0E7U3Y3FLepmRPI-y1zR78qxE4fTv4Raexalc4XMnwRtPgfPgZ9HSyzc3PsHagY_3pdVyjjdTyJsn52xhcy5Bj_oZKMDZ_Ct8btoLXHmWy9pRsNZBcpj2pt7m_xYP5DD2t572of54-Qc78byS7jKjv27b6qbAqom9RIuhKlJC2zXGd3SiMHfDk4QaTxRpBA0_s9hfnDDvUenrs6WgbqSToIH7Ak-h2HAZtPB-ihwMBM3675ln5KfJWcW49MOGX-TM4yS9L_wO4Sp5_weHnvNQ&q="
