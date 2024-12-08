Start redis:
  $ flock /tmp/redis-test.lock ../run_redis.sh --dir $PWD --dbfilename dump.rdb


GET key from rdb file:
  $ redis-cli GET hello
  world1

Kill redis:
  $ ../kill_redis.sh
