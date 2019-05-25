#!/bin/sh
prefix="./public/.well-known/acme-challenge"
if test -n "$ACME_VALIDATION" -a -n "$ACME_TOKEN"; then
	mkdir -p "$prefix"
	echo "$ACME_VALIDATION" >"$prefix/$ACME_TOKEN"
fi
