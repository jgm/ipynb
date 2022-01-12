# Revision history for ipynb

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
