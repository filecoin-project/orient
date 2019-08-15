input_dir=$1
output=$input_dir/`jq -rs '.[0].git.hash' $input_dir/zigzag-benchmarks.json`-benchmarks.json
jq -s '.[0] * {benchmarks: {"hash-constraints": .[0].benchmarks, "zigzag-benchmarks": .[1].benchmarks, "micro-benchmarks": [.[2].benchmarks]}}' $input_dir/hash-constraints.json $input_dir/zigzag-benchmarks.json $input_dir/micro-benchmarks.json > $output
