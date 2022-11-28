For latest version of this document see [`ChangeLog.md on GitHub`](https://github.com/trskop/http-rfc7807/blob/main/ChangeLog.md).

### HEAD (unreleased changes)

* (**change**) Update aeson dependency to v2.

* (**breaking change**) Removed support for `servant-server-0.15`.

* (**change**) Introduces module `Network.WAI.RFC7807` which provides
  `rfc7807Response`, a way how to create WAI `Response` in similar way as
  `rfc7807ServerError`.

* (**change**) Documentation fixes, mostly Haddock syntax related.


### 0.2.0.0

* (**breaking change**) Function `rfc7807ServerError` sets the value of `title`
  field is set to HTTP reason from Servant's `ServerError` by default. User can
  still override it as before, but this is a breaking change as it changes the
  expected output.

* (**breaking change**) Type variable `body` is now first in the definition of
  `rfc7807ServerError`. If you've used `TypeApplications` when calling
  `rfc7807ServerError` function then this may be a breaking change for you.

* (**change**) Support `servant-server-0.15` in addition to currently supported
  versions. We are planning to drop the support once again in next major
  release. See [`servant-server-1.16` ChangeLog
  ](https://hackage.haskell.org/package/servant-server-0.16/changelog) to see
  what it entails.

* Documentation updates


### 0.1.0.0

Initial release
