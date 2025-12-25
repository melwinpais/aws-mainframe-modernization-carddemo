package com.carddemo.repository;

import com.carddemo.entity.Card;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * JPA Repository for Card entity
 * Provides data access methods for the cards table
 */
@Repository
public interface CardRepository extends JpaRepository<Card, Long> {

    /**
     * Find card by card number
     * @param cardNumber the card number to search for
     * @return Optional containing the card if found
     */
    Optional<Card> findByCardNumber(Long cardNumber);

    /**
     * Find all cards by account ID
     * @param accountId the account ID to search for
     * @return List of cards associated with the account
     */
    List<Card> findByAccountId(Long accountId);

    /**
     * Find all cards by customer ID
     * @param customerId the customer ID to search for
     * @return List of cards associated with the customer
     */
    List<Card> findByCustomerId(Long customerId);

    /**
     * Find all cards by card status
     * @param cardStatus the card status to search for
     * @return List of cards with the specified status
     */
    List<Card> findByCardStatus(String cardStatus);

    /**
     * Find all cards by account ID and card status
     * @param accountId the account ID to search for
     * @param cardStatus the card status to search for
     * @return List of cards matching both criteria
     */
    List<Card> findByAccountIdAndCardStatus(Long accountId, String cardStatus);

    /**
     * Check if a card exists by card number
     * @param cardNumber the card number to check
     * @return true if card exists, false otherwise
     */
    boolean existsByCardNumber(Long cardNumber);

    /**
     * Count cards by account ID
     * @param accountId the account ID to count cards for
     * @return Number of cards associated with the account
     */
    long countByAccountId(Long accountId);

    /**
     * Find cards expiring before specified date
     * @param expirationDate the expiration date threshold (YYYY-MM-DD format)
     * @return List of cards expiring before the specified date
     */
    @Query("SELECT c FROM Card c WHERE c.expirationDate < :expirationDate")
    List<Card> findCardsExpiringBefore(@Param("expirationDate") String expirationDate);

    /**
     * Find active cards by account ID
     * @param accountId the account ID to search for
     * @param activeStatus the active status to filter by
     * @return List of active cards for the account
     */
    @Query("SELECT c FROM Card c WHERE c.accountId = :accountId AND c.cardStatus = :activeStatus")
    List<Card> findActiveCardsByAccount(@Param("accountId") Long accountId, 
                                        @Param("activeStatus") String activeStatus);
}
