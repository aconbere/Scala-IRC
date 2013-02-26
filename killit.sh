#!/bin/sh
ps aux | grep java | grep Main | awk '{print }' | xargs kill
