package com.carddemo.repository;

import com.carddemo.entity.Customer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * JPA Repository for Customer entity
 * Provides data access methods for the customers table
 */
@Repository
public interface CustomerRepository extends JpaRepository<Customer, Long> {

    /**
     * Find customer by customer ID
     * @param customerId the customer ID to search for
     * @return Optional containing the customer if found
     */
    Optional<Customer> findByCustomerId(Long customerId);

    /**
     * Find customers by last name
     * @param lastName the last name to search for
     * @return List of customers with the specified last name
     */
    List<Customer> findByLastName(String lastName);

    /**
     * Find customers by last name and first name
     * @param lastName the last name to search for
     * @param firstName the first name to search for
     * @return List of customers with the specified name
     */
    List<Customer> findByLastNameAndFirstName(String lastName, String firstName);

    /**
     * Find customer by SSN
     * @param ssn the SSN to search for
     * @return Optional containing the customer if found
     */
    Optional<Customer> findBySsn(Long ssn);

    /**
     * Find customers by ZIP code
     * @param zipCode the ZIP code to search for
     * @return List of customers in the specified ZIP code
     */
    List<Customer> findByZipCode(String zipCode);

    /**
     * Find customers by state code
     * @param stateCode the state code to search for
     * @return List of customers in the specified state
     */
    List<Customer> findByStateCode(String stateCode);

    /**
     * Check if a customer exists by customer ID
     * @param customerId the customer ID to check
     * @return true if customer exists, false otherwise
     */
    boolean existsByCustomerId(Long customerId);

    /**
     * Check if a customer exists by SSN
     * @param ssn the SSN to check
     * @return true if customer with SSN exists, false otherwise
     */
    boolean existsBySsn(Long ssn);

    /**
     * Find customers with FICO score in specified range
     * @param minScore the minimum FICO score
     * @param maxScore the maximum FICO score
     * @return List of customers with FICO score in range
     */
    @Query("SELECT c FROM Customer c WHERE c.ficoCreditScore BETWEEN :minScore AND :maxScore")
    List<Customer> findCustomersWithFicoScoreInRange(@Param("minScore") Integer minScore, 
                                                      @Param("maxScore") Integer maxScore);
}
