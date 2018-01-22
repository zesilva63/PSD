package models;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.base.Optional;

import java.util.Random;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class Company {
    private String name;
    private Exchange exchange;
    private Stock yesterday, today;

    public Company(String name) {
        this.name = name;

        Optional<Integer> initialValue = generateInitialStocks();
        yesterday = new Stock(initialValue);
        today = yesterday.close();

        exchange = new Exchange();
    }

    public String getName() {
        return name;
    }

    public Exchange getExchange() {
        return exchange;
    }

    public Stock getYesterday() {
        if (yesterday.hasTransactions())
            return yesterday;

        return null;
    }

    public Stock getToday() {
        if (today.hasTransactions())
            return today;

        return null;
    }

    public void accept(Transaction tr) {
        today.apply(tr);
    }

    public void close() {

    }

    private Optional<Integer> generateInitialStocks() {
        Random r = new Random();
        int value = r.nextInt(10);

        if (value == 0)
            Optional.absent();

        return Optional.of(value);
    }
}