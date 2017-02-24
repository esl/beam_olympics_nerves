#!/bin/sh

echo "Changing working directory to tmp"
cd /tmp
echo "Starting Erlang/OTP..."
exec $*
