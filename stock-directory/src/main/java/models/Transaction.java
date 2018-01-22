package models;

import com.fasterxml.jackson.annotation.JsonCreator;

public class Transaction {
    private String company;
    private int quantity, price;

    @JsonCreator
    public Transaction(String company, int quantity, int price) {
        this.company = company;
        this.quantity = quantity;
        this.price = price;
    }

    public String getCompany() {
        return company;
    }

    public int getQuantity() {
        return quantity;
    }

    public int getPrice() {
        return price;
    }
}
