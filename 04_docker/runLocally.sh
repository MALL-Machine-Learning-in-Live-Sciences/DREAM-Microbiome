docker run \
  --rm \
  --network none \
  -v $PWD/training/:/input/:ro \
  -v $PWD/output/:/output/ \
  test bash
