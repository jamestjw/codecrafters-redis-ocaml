KEYS fetches all keys from the store that are not expired

Start redis:
  $ flock /tmp/redis-test.lock ./run_redis.sh

KEYS with empty store:
  $ redis-cli KEYS "*"
  

KEYS with after storing something:
  $ redis-cli SET test-key 1997
  OK
  $ redis-cli KEYS "*"
  test-key

KEYS with multiple keys:
  $ redis-cli SET other-key1 some-value1
  OK
  $ redis-cli SET other-key2 some-value2
  OK
  $ redis-cli SET other-key3 some-value3
  OK
  $ redis-cli KEYS "*"
  other-key1
  other-key2
  other-key3
  test-key

KEYS with star:
  $ redis-cli KEYS "test*"
  test-key
  $ redis-cli KEYS "*test*"
  test-key
  $ redis-cli KEYS "*other*"
  other-key1
  other-key2
  other-key3
  $ redis-cli KEYS "*key"
  test-key
  $ redis-cli KEYS "*key*"
  other-key1
  other-key2
  other-key3
  test-key

KEYS with wildcard
  $ redis-cli KEYS "*k?y*"
  other-key1
  other-key2
  other-key3
  test-key
  $ redis-cli KEYS "*key?"
  other-key1
  other-key2
  other-key3

KEYS with char set
  $ redis-cli KEYS "*key[12]"
  other-key1
  other-key2

KEYS with exclusion
  $ redis-cli KEYS "*key[^1]"
  other-key2
  other-key3

KEYS with char range using digits:
  $ redis-cli KEYS "*key[1-2]"
  other-key1
  other-key2

KEYS with char range using chars:
  $ redis-cli SET key_a 1
  OK
  $ redis-cli SET key_b 1
  OK
  $ redis-cli SET key_c 1
  OK
  $ redis-cli SET key_d 1
  OK
  $ redis-cli KEYS "key_[a-c]"
  key_a
  key_b
  key_c

Kill redis:
  $ ./kill_redis.sh
