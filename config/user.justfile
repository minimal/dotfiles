broker := "localhost:9092"

default:
    @just --list --justfile {{ justfile() }}

clj-deps-tree:
    clojure -X:deps tree

clj-antq:
    clojure -Sdeps '{:deps {com.github.liquidz/antq {:mvn/version "RELEASE"}}}' -M -m antq.core

# confluent tools kafka status
kafka-status:
    confluent local services status

kafka-start:
    confluent local services schema-registry start

kafka-destroy:
    confluent local destroy

kafka-list-topics:
    kafka-topics --list --bootstrap-server {{ broker }}

kafka-consume-avro-topic topic:
    kafka-avro-console-consumer --topic {{ topic }} --property schema.registry.url="http://127.0.0.1:8081" --property print.key=true  --key-deserializer=org.apache.kafka.common.serialization.StringDeserializer --from-beginning --bootstrap-server {{ broker }}

kafka-delete-topic topic:
    kafka-topics --bootstrap-server {{ broker }} --delete --topic {{ topic }}

elgato-base payload:
    /Users/chris.mcdevitt/.nix-profile/bin/http PUT http://192.168.0.38:9123/elgato/lights  <<< '{"numberOfLights":1,"lights":[{{ payload }}]}'

elgato-on: (elgato-base '{"on":1}')

elgato-off: (elgato-base '{"on":0}')

elgato-morning-zoom: (elgato-base '{"temperature": 100, "brightness": 31, "on": 1}')

elgato-morning-low: (elgato-base '{"temperature": 100, "brightness": 9, "on": 1}')

elgato-evening-low: (elgato-base '{"temperature": 400, "brightness": 9, "on": 1}')

get-my-ip:
    curl ifconfig.co
