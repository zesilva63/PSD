package exchange;

import java.util.*;

public class Company {

	private List<Order> buyOrders;
	private Queue<Order> sellOrders;
	private Queue<Transactions> transactions;

	public Company() {
		this. transactions = new ArrayList<>();
		this.buyOrders = new PriorityQueue<>();
		this.sellOrders = new PriorityQueue<>();
	}

	public addOrder(Order o) {
		
	}

	Comparator<Order> byPrice = (Order o1, Order o2) -> o1.getPrice().compareTo(o2.getPrice());

}
