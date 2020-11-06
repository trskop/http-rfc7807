#!/usr/bin/env bash

# So called Bash strict mode.  See `bash(1)` documentation for more details.
set -eo pipefail

if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) )); then
    # Treat unset variables and parameters as an error when expanding.  This
    # wasn't very reliable in older Bash versions, hence the version check.
    set -u

    # Enable the behaviour of 'set -e' inside command substitution.  This
    # option is available since Bash 4.4.
    shopt -s inherit_errexit
fi

# Set RELEASE_DEBUG=1 to enable debugging information.
if (( ${RELEASE_DEBUG:-0} )); then
    # This will cause Bash to print commands before executing them.
    set -x
fi

function usage() {
    cat <<EOF
Do a release of 'http-rfc7807' package.

Usage:

  ${BASH_SOURCE[0]##*/} VERSION
  ${BASH_SOURCE[0]##*/} [--help|-h]

Changes made by this script can be reverted. Please, be aware that the
following instructions work only if there were no additional changes made. Use
your judgement when executing them:

  git reset --hard HEAD^
  git tag --delete VERSION  # Use the same VERSION as we just created
EOF
}

function main() {
    if [[ $# -eq 0 || "$1" =~ ^(-h|--help)$ ]]; then
        usage
        exit 0
    fi

    local -r version="$1"; shift
    if [[ -z "${version}" || ! "${version}" =~ ^([0-9]+\.)*[0-9]+$ ]]; then
        usage 1>&2
        exit 1
    fi

    local root=
    root="$(realpath "$(dirname "${BASH_SOURCE[0]:?"Call the script directly"}")")"

    local -r versionFile="${root}/version.yaml"
    local -r changeLogFile="${root}/ChangeLog.md"
    local -r readmeFile="${root}/README.md"
    local -r -a updatedFiles=(
        "${versionFile}"
        "${changeLogFile}"
        "${readmeFile}"
        "${root}/http-rfc7807.cabal"
        "${root}/stack.yaml"
        "${root}/stack.yaml.lock"
    )

    local commit=''
    commit="$(git rev-parse --verify HEAD)"

    echo "root=${root}"
    echo "version=${version}"

    echo "\"${version}\"" > "${versionFile}"

    sed --in-place \
        "s/HEAD (unreleased changes)/${version}/" \
        "${changeLogFile}"

    # Tweak README.md to work on Hackage correctly.
    sed --in-place \
        "s/](\.\//](https:\/\/raw.githubusercontent.com\/trskop\/http-rfc7807\/${commit}/" \
        "${readmeFile}"

    (
        cd "${root}" || exit 1
        stack test
    )

    git -C "${root}" add "${updatedFiles[@]}"
    git -C "${root}" commit -m "Release ${version}"
    git -C "${root}" tag "${version}"

    (
        cd "${root}" || exit 1
        stack sdist
    )
}

main "$@"
