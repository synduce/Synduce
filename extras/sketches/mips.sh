#!/bin/bash
{ racket mips_v0.rkt; } &
{ racket mips_v1.rkt; } &
{ racket mips_v2.rkt; } &
{ racket mips_v3.rkt; } &
wait -n
pkill -P $$
