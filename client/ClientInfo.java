package client;
import java.util.ArrayDeque;
import client.*;

public class ClientInfo {
	private int command;
	private int reply;
	private boolean logged;
	private boolean success;
	private String content;
	private ArrayDeque<String> notifications;
	private ArrayDeque<String> transactions;

	ClientInfo() {
		command = -1;
		reply = 1;
		logged = false;
		success = false;
		content = null;
		notifications = new ArrayDeque<>();
		transactions = new ArrayDeque<>();
	}

	synchronized public int getCommand() {
		return command;
	}

	synchronized public void setCommand(int command) {
		this.command = command;
	}

	synchronized public boolean isLogged() {
		return logged;
	}

	synchronized public void setLogged(boolean status) {
		logged = status;
	}

	synchronized public void setReply(boolean success, String content) {
		this.success = success;
		this.content = content;
System.out.println("A maria foi " + Boolean.toString(success));
		reply = 1 - reply;
		notifyAll();
	}

	synchronized public void addNotification(String message) {
		notifications.addLast(message);
	}

	synchronized public void addTransaction(String message) {
		transactions.addLast(message);
	}

	synchronized public boolean getReplyStatus() {
		return success;
	}

	synchronized public String getResponse() {
		int myReply = reply;

		while(myReply == reply) {
			try {
				wait();
			} catch (InterruptedException e) {
				continue;
			}
		}

		return content;
	}

	synchronized public String getNotifications() {
		String msg;
		StringBuilder sb = new StringBuilder();

		while((msg = notifications.pollFirst()) != null)
			sb.append(msg).append("\n");

		if (sb.length() > 0)
			sb.deleteCharAt(sb.length() - 1);

		return sb.toString();
	}

	synchronized public String getTransactions() {
		String msg;
		StringBuilder sb = new StringBuilder();

		while((msg = transactions.pollFirst()) != null)
			sb.append(msg).append("\n");

		if (sb.length() > 0)
			sb.deleteCharAt(sb.length() - 1);

		return sb.toString();
	}

	synchronized public int getNumberOfNotifications() {
		return notifications.size();
	}

	synchronized public int getNumberOfTransactions() {
		return transactions.size();
	}

}
