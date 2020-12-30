#! /bin/sh

inc=${1:-1}
num_devices=$(pacmd list-sinks | grep -c index:)
sink_arr=($(pacmd list-sinks | grep index: | grep -o '[0-9]\+'))
default_sink_index=$(( $(pacmd list-sinks | grep index: | grep -no '*' | grep -o '^[0-9]\+') - 1 ))
default_sink_index=$(( ($default_sink_index + $num_devices + $inc) % $num_devices ))
default_sink=${sink_arr[$default_sink_index]}
pacmd set-default-sink $default_sink
pacmd list-sink-inputs | grep index: | grep -o '[0-9]\+' | while read SINK
do
    pacmd move-sink-input $SINK $default_sink
done
