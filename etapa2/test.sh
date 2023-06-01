cat test_input.txt | ./etapa1 > test_output.txt &&
if diff -q test_output.txt test_expected_output.txt &>/dev/null; then
  >&2 echo "Passou"
fi &&
rm test_output.txt