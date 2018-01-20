client:
	protoc --java_out=. protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar client/*.java

frontend:
	dependencies/gpb/bin/protoc-erl -I. -maps -o frontend/ protos/protocol.proto
	erlc -I dependencies/gpb/include -o frontend/ frontend/protocol.erl
	erlc -I dependencies/erlzmq2-master/include -o frontend/ frontend/erlzmq.erl
	erlc -I dependencies/erlzmq2-master/include -o frontend/ frontend/erlzmq_nif.erl

exchange:
	protoc --java_out=. protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:. exchange/*.java


runcli:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:. client.Client localhost

run-exchange:
	java -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:. exchange.Exchange

clean:
	rm client/*.class
	rm frontend/*.beam
	rm frontend/protocol.erl
	rm client/Protocol.java
	rm exchange/Protocol.java