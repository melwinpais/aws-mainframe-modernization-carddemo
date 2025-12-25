package com.carddemo.repository;

import com.carddemo.entity.Transaction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * JPA Repository for Transaction entity
 * Provides data access methods for the transactions table
 */
@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {

    /**
     * Find transaction by transaction ID
     * @param transactionId the transaction ID to search for
     * @return Optional containing the transaction if found
     */
    Optional<Transaction> findByTransactionId(Long transactionId);

    /**
     * Find all transactions by account ID with pagination
     * @param accountId the account ID to search for
     * @param pageable pagination information
     * @return Page of transactions for the account
     */
    Page<Transaction> findByAccountId(Long accountId, Pageable pageable);

    /**
     * Find all transactions by card number with pagination
     * @param cardNumber the card number to search for
     * @param pageable pagination information
     * @return Page of transactions for the card
     */
    Page<Transaction> findByCardNumber(Long cardNumber, Pageable pageable);

    /**
     * Find all transactions by transaction type
     * @param transactionType the transaction type to search for
     * @return List of transactions with the specified type
     */
    List<Transaction> findByTransactionType(String transactionType);

    /**
     * Find transactions by account ID and date range with pagination
     * @param accountId the account ID to search for
     * @param startDate the start date (YYYY-MM-DD format)
     * @param endDate the end date (YYYY-MM-DD format)
     * @param pageable pagination information
     * @return Page of transactions within the date range
     */
    @Query("SELECT t FROM Transaction t WHERE t.accountId = :accountId " +
           "AND t.transactionDate >= :startDate AND t.transactionDate <= :endDate " +
           "ORDER BY t.transactionDate DESC, t.transactionId DESC")
    Page<Transaction> findByAccountIdAndDateRange(@Param("accountId") Long accountId,
                                                   @Param("startDate") String startDate,
                                                   @Param("endDate") String endDate,
                                                   Pageable pageable);

    /**
     * Find transactions by card number and date range with pagination
     * @param cardNumber the card number to search for
     * @param startDate the start date (YYYY-MM-DD format)
     * @param endDate the end date (YYYY-MM-DD format)
     * @param pageable pagination information
     * @return Page of transactions within the date range
     */
    @Query("SELECT t FROM Transaction t WHERE t.cardNumber = :cardNumber " +
           "AND t.transactionDate >= :startDate AND t.transactionDate <= :endDate " +
           "ORDER BY t.transactionDate DESC, t.transactionId DESC")
    Page<Transaction> findByCardNumberAndDateRange(@Param("cardNumber") Long cardNumber,
                                                    @Param("startDate") String startDate,
                                                    @Param("endDate") String endDate,
                                                    Pageable pageable);

    /**
     * Find transactions by account ID and transaction type
     * @param accountId the account ID to search for
     * @param transactionType the transaction type to search for
     * @return List of transactions matching both criteria
     */
    List<Transaction> findByAccountIdAndTransactionType(Long accountId, String transactionType);

    /**
     * Count transactions by account ID
     * @param accountId the account ID to count transactions for
     * @return Number of transactions for the account
     */
    long countByAccountId(Long accountId);

    /**
     * Calculate total transaction amount by account ID and transaction type
     * @param accountId the account ID to calculate for
     * @param transactionType the transaction type to filter by
     * @return Total amount of transactions
     */
    @Query("SELECT SUM(t.amount) FROM Transaction t WHERE t.accountId = :accountId " +
           "AND t.transactionType = :transactionType")
    BigDecimal sumAmountByAccountIdAndType(@Param("accountId") Long accountId,
                                           @Param("transactionType") String transactionType);

    /**
     * Find recent transactions by account ID
     * @param accountId the account ID to search for
     * @param limit the maximum number of transactions to return
     * @return List of recent transactions
     */
    @Query("SELECT t FROM Transaction t WHERE t.accountId = :accountId " +
           "ORDER BY t.transactionDate DESC, t.transactionId DESC")
    List<Transaction> findRecentTransactionsByAccount(@Param("accountId") Long accountId, 
                                                      Pageable pageable);
}
