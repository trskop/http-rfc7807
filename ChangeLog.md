For latest version of this document see [`ChangeLog.md on GitHub`](https://github.com/trskop/http-rfc7807/blob/main/ChangeLog.md).

### HEAD (unreleased changes)

* (**breaking change**) Function `rfc7807ServerError` sets the value of `title`
  field is set to HTTP reason from Servant's `ServerError` by default. User can
  still override it as before, but this is a breaking change as it changes the
  expected output.

* Documentation updates

### 0.1.0.0

Initial release
