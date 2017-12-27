package client;

import com.google.protobuf.CodedInputStream;
import com.google.protobuf.CodedOutputStream;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import client.Protocol.*;


public class User {

	public static void main(String[] args) {
		try{	
			Socket s = new Socket("localhost", 6363);
			InputStream in = s.getInputStream();
			OutputStream out = s.getOutputStream();
			CodedInputStream cis = CodedInputStream.newInstance(in);
    		CodedOutputStream cos = CodedOutputStream.newInstance(out);
    
			AuthenticationRequest req = AuthenticationRequest.newBuilder().setUsername("the63x").setPassword("pass").setRequest(AuthenticationRequest.RequestType.REGISTRATION).build();
			
			byte[] ba = req.toByteArray();
    		cos.writeUInt32NoTag(ba.length);
      		cos.flush();
      		cos.writeRawBytes(ba);

			int len = cis.readRawVarint32();
			ba = cis.readRawBytes(len);
			AuthenticationResponse b = AuthenticationResponse.parseFrom(ba);
			Printer.print(b);

		}
		catch(Exception e){
			e.printStackTrace();
			System.exit(0);
		}
	}
}
