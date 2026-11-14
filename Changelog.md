# Revision history for ipynb

## 0.3 -- 2026-11-14

* Allow null `execution_count` in execute_result [API change].
  The v4 schema allows `execution_count` to be null in an
  execute_result output, but such notebooks failed to parse.
  Change the type of `executeCount` from Int to Maybe Int.

* Don't drop svg, json, pdf, and other mime data in v3 outputs.

* Keep non-textual mime data as JsonData instead of failing.

* Only base64-decode mime data for binary mime types.

* Remove unneeded CPP, require aeson >= 2 and base >= 4.11.

* Update Cabal file to cabal version 2.0

* Remove stack.yaml.

## 0.2 -- 2022-01-11

* Make MimeAttachments a newtype, and export it [API change].
  Derive ToJSON, ToEncoding using generics for deterministic field order.

* Add Ord instance for MimeBundle and MimeData. [API change]

* Derive ToJSON for MimeBundle, with a deterministic order of keys.

* Add ToJSON instance for MimeData [API change].

* Make JSONMeta a newtype and export it [API change].
  Derive ToJSON with a deterministic order of keys.

* Add cellId field to Cell (#2). [API change] Needed for Nb4.5+.

## 0.1.0.2 -- 2020-10-23

* Changes for aeson 2 compatibility.

* Simplified round trip test to avoid use of aeson-diff, which doesn't work
  with aeson 2.

## 0.1.0.1 -- 2020-04-25

* Fixed to build with base64-bytestring 1.1.

## 0.1.0.0 -- 2019-01-22

* Initial release.
