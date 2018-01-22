package resources;

import models.Company;
import models.Transaction;

import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Path("/")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class CompanyResource {
    private final Map<String, Company> companies;

    public CompanyResource() {
        companies = new HashMap<>();

        companies.put("Gota", new Company("Gota"));
        companies.put("Peões", new Company("Peões"));
        companies.put("Speedy", new Company("Speedy"));
        companies.put("Diobar", new Company("Diobar"));
    }

    @GET
    @Path("/companies")
    public List<String> getCompanies() {
        return companies.values()
                .stream()
                .map(Company::getName)
                .collect(Collectors.toList());
    }

    @GET
    @Path("/company/{name}")
    public Company getCompany(@PathParam("name") String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format("Company %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return company;
    }

    @PUT
    @Path("/company/{name}")
    public Response registerTransaction(@PathParam("name") String name, @NotNull Transaction tr) {
        Company company = companies.get(name);
        company.accept(tr);

        return Response.noContent().build();
    }
}