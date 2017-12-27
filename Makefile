client:
	protoc --java_out=. protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar client/*.java

frontend:
	./dependencies/gpb/bin/protoc-erl protos/protocol.proto
	mv protos/protocol.erl frontend/
	mv protos/protocol.hrl frontend/
	erlc -I dependencies/gpb/include -o frontend/ frontend/protocol.erl

runcli:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar: client.Client localhost

clean:
	rm client/*.class
	rm frontend/*.beam
	rm frontend/*.hrl
	rm frontend/protocol.erl
	rm client/Protocol.java
