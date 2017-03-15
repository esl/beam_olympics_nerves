#!/bin/sh

echo "Changing working directory to /root"
cd /root
echo "Starting Erlang/OTP..."
exec $*
