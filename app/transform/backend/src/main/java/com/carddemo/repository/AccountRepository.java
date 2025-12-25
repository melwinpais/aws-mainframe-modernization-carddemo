package com.carddemo.repository;

import com.carddemo.entity.Account;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * JPA Repository for Account entity
 * Provides data access methods for the accounts table
 */
@Repository
public interface AccountRepository extends JpaRepository<Account, Long> {

    /**
     * Find account by account ID
     * @param accountId the account ID to search for
     * @return Optional containing the account if found
     */
    Optional<Account> findByAccountId(Long accountId);

    /**
     * Find all accounts by active status
     * @param activeStatus the active status ('Y' or 'N')
     * @return List of accounts with the specified status
     */
    List<Account> findByActiveStatus(String activeStatus);

    /**
     * Find all accounts by address ZIP code
     * @param addressZip the ZIP code to search for
     * @return List of accounts in the specified ZIP code
     */
    List<Account> findByAddressZip(String addressZip);

    /**
     * Find all accounts by group ID
     * @param groupId the group ID to search for
     * @return List of accounts in the specified group
     */
    List<Account> findByGroupId(String groupId);

    /**
     * Check if an account exists by account ID
     * @param accountId the account ID to check
     * @return true if account exists, false otherwise
     */
    boolean existsByAccountId(Long accountId);

    /**
     * Find accounts with balance greater than specified amount
     * @param balance the minimum balance
     * @return List of accounts with balance greater than specified amount
     */
    @Query("SELECT a FROM Account a WHERE a.currentBalance > :balance")
    List<Account> findAccountsWithBalanceGreaterThan(@Param("balance") java.math.BigDecimal balance);

    /**
     * Find accounts with credit limit less than specified amount
     * @param creditLimit the maximum credit limit
     * @return List of accounts with credit limit less than specified amount
     */
    @Query("SELECT a FROM Account a WHERE a.creditLimit < :creditLimit")
    List<Account> findAccountsWithCreditLimitLessThan(@Param("creditLimit") java.math.BigDecimal creditLimit);
}
