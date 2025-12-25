package com.carddemo.service;

import com.carddemo.dto.CardCreateDto;
import com.carddemo.dto.CardDto;
import com.carddemo.dto.CardUpdateDto;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Customer;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.CustomerRepository;
import net.jqwik.api.*;
import org.mockito.ArgumentCaptor;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

/**
 * Property-based tests for CardService
 * Tests universal properties across all inputs
 * Validates Requirements 5.1, 5.2, 5.3, 5.4, 17.2
 */
class CardServicePropertyTest {

    private CardRepository cardRepository;
    private AccountRepository accountRepository;
    private CustomerRepository customerRepository;
    private ValidationService validationService;
    private CardServiceImpl cardService;

    /**
     * Property Test: Card Number Validation (16 digits)
     * For any card number input, the system should validate that it is exactly 16 digits,
     * rejecting invalid card numbers with appropriate error messages
     * Validates: Requirements 17.2
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Card Number Validation")
    void testCardNumberValidation(@ForAll("cardNumbers") Long cardNumber) {
        setupMocks();
        
        // Test card number validation through createCard
        CardCreateDto cardData = new CardCreateDto();
        cardData.setCardNumber(cardNumber);
        cardData.setAccountId(12345678901L);
        cardData.setCustomerId(123456789L);
        cardData.setCardStatus("ACTIVE");
        
        // Mock account and customer exist
        Account mockAccount = new Account();
        mockAccount.setAccountId(12345678901L);
        when(accountRepository.findById(anyLong())).thenReturn(Optional.of(mockAccount));
        
        Customer mockCustomer = new Customer();
        mockCustomer.setCustomerId(123456789L);
        when(customerRepository.findById(anyLong())).thenReturn(Optional.of(mockCustomer));
        
        // Check if card number is valid (16 digits)
        String cardNumberStr = String.valueOf(cardNumber);
        boolean isValid = cardNumberStr.length() == 16 && cardNumberStr.matches("\\d{16}");
        
        if (isValid) {
            // Valid card number - should not throw validation exception for format
            when(cardRepository.existsByCardNumber(cardNumber)).thenReturn(false);
            when(cardRepository.save(any(Card.class))).thenAnswer(invocation -> invocation.getArgument(0));
            
            // Should succeed or fail for other reasons (not format)
            try {
                CardDto result = cardService.createCard(cardData);
                assertThat(result).isNotNull();
                assertThat(result.getCardNumber()).isEqualTo(cardNumber);
            } catch (ValidationException e) {
                // If validation fails, it should not be about the 16-digit format
                assertThat(e.getMessage()).doesNotContain("must be exactly 16 digits");
            }
        } else {
            // Invalid card number - should throw validation exception
            assertThatThrownBy(() -> cardService.createCard(cardData))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("16 digits");
        }
    }

    /**
     * Property Test: Card Retrieval by Card Number
     * For any valid card number, when retrieving card data, the system should return
     * the card record if it exists, or throw ResourceNotFoundException if it doesn't
     * Validates: Requirements 5.1
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Card Retrieval by Card Number")
    void testCardRetrievalByCardNumber(@ForAll("validCardNumbers") Long cardNumber) {
        setupMocks();
        
        // Test case 1: Card exists
        Card mockCard = new Card();
        mockCard.setCardNumber(cardNumber);
        mockCard.setAccountId(12345678901L);
        mockCard.setCustomerId(123456789L);
        mockCard.setCardStatus("ACTIVE");
        mockCard.setExpirationDate("2025-12-31");
        mockCard.setIssueDate("2023-01-01");
        
        when(cardRepository.findByCardNumber(cardNumber)).thenReturn(Optional.of(mockCard));
        
        CardDto result = cardService.getCard(cardNumber);
        
        // Verify card data is returned correctly
        assertThat(result).isNotNull();
        assertThat(result.getCardNumber()).isEqualTo(cardNumber);
        assertThat(result.getAccountId()).isEqualTo(12345678901L);
        assertThat(result.getCustomerId()).isEqualTo(123456789L);
        assertThat(result.getCardStatus()).isEqualTo("ACTIVE");
        
        // Test case 2: Card does not exist
        Long nonExistentCardNumber = cardNumber + 1;
        when(cardRepository.findByCardNumber(nonExistentCardNumber)).thenReturn(Optional.empty());
        
        assertThatThrownBy(() -> cardService.getCard(nonExistentCardNumber))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("Card not found");
    }

    /**
     * Property Test: Card Retrieval by Account ID
     * For any account ID, when retrieving cards, the system should return all cards
     * associated with that account, or throw ResourceNotFoundException if account doesn't exist
     * Validates: Requirements 5.2
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Card Retrieval by Account ID")
    void testCardRetrievalByAccountId(@ForAll("accountIds") Long accountId) {
        setupMocks();
        
        // Test case 1: Account exists with cards
        when(accountRepository.existsById(accountId)).thenReturn(true);
        
        Card card1 = new Card();
        card1.setCardNumber(1234567890123456L);
        card1.setAccountId(accountId);
        card1.setCustomerId(123456789L);
        card1.setCardStatus("ACTIVE");
        
        Card card2 = new Card();
        card2.setCardNumber(6543210987654321L);
        card2.setAccountId(accountId);
        card2.setCustomerId(987654321L);
        card2.setCardStatus("INACTIVE");
        
        List<Card> mockCards = Arrays.asList(card1, card2);
        when(cardRepository.findByAccountId(accountId)).thenReturn(mockCards);
        
        List<CardDto> results = cardService.getCardsByAccount(accountId);
        
        // Verify all cards are returned
        assertThat(results).isNotNull();
        assertThat(results).hasSize(2);
        assertThat(results.get(0).getAccountId()).isEqualTo(accountId);
        assertThat(results.get(1).getAccountId()).isEqualTo(accountId);
        
        // Test case 2: Account does not exist
        Long nonExistentAccountId = accountId + 1;
        when(accountRepository.existsById(nonExistentAccountId)).thenReturn(false);
        
        assertThatThrownBy(() -> cardService.getCardsByAccount(nonExistentAccountId))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("Account not found");
        
        // Test case 3: Account exists but has no cards
        Long emptyAccountId = accountId + 2;
        when(accountRepository.existsById(emptyAccountId)).thenReturn(true);
        when(cardRepository.findByAccountId(emptyAccountId)).thenReturn(Arrays.asList());
        
        List<CardDto> emptyResults = cardService.getCardsByAccount(emptyAccountId);
        assertThat(emptyResults).isNotNull();
        assertThat(emptyResults).isEmpty();
    }

    /**
     * Property Test: Card Creation with Valid Data
     * For any valid card creation data, the system should create a new card
     * and persist it to the database, returning the created card details
     * Validates: Requirements 5.3
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Card Creation with Valid Data")
    void testCardCreationWithValidData(
            @ForAll("validCardNumbers") Long cardNumber,
            @ForAll("accountIds") Long accountId,
            @ForAll("customerIds") Long customerId,
            @ForAll("cardStatuses") String cardStatus) {
        setupMocks();
        
        // Create card data
        CardCreateDto cardData = new CardCreateDto();
        cardData.setCardNumber(cardNumber);
        cardData.setAccountId(accountId);
        cardData.setCustomerId(customerId);
        cardData.setCardStatus(cardStatus);
        cardData.setExpirationDate("2025-12-31");
        cardData.setIssueDate("2023-01-01");
        
        // Mock card doesn't exist yet
        when(cardRepository.existsByCardNumber(cardNumber)).thenReturn(false);
        
        // Mock account exists
        Account mockAccount = new Account();
        mockAccount.setAccountId(accountId);
        mockAccount.setActiveStatus("Y");
        mockAccount.setCurrentBalance(BigDecimal.ZERO);
        mockAccount.setCreditLimit(BigDecimal.valueOf(10000));
        mockAccount.setCashCreditLimit(BigDecimal.valueOf(5000));
        when(accountRepository.findById(accountId)).thenReturn(Optional.of(mockAccount));
        
        // Mock customer exists
        Customer mockCustomer = new Customer();
        mockCustomer.setCustomerId(customerId);
        mockCustomer.setFirstName("Test");
        mockCustomer.setLastName("Customer");
        when(customerRepository.findById(customerId)).thenReturn(Optional.of(mockCustomer));
        
        // Mock validation service
        ValidationResult validResult = ValidationResult.success();
        when(validationService.validateDate(anyString())).thenReturn(validResult);
        
        // Mock save operation
        ArgumentCaptor<Card> cardCaptor = ArgumentCaptor.forClass(Card.class);
        when(cardRepository.save(cardCaptor.capture())).thenAnswer(invocation -> invocation.getArgument(0));
        
        // Create card
        CardDto result = cardService.createCard(cardData);
        
        // Verify card was created correctly
        assertThat(result).isNotNull();
        assertThat(result.getCardNumber()).isEqualTo(cardNumber);
        assertThat(result.getAccountId()).isEqualTo(accountId);
        assertThat(result.getCustomerId()).isEqualTo(customerId);
        assertThat(result.getCardStatus()).isEqualTo(cardStatus);
        assertThat(result.getExpirationDate()).isEqualTo("2025-12-31");
        assertThat(result.getIssueDate()).isEqualTo("2023-01-01");
        
        // Verify card was saved to repository
        Card savedCard = cardCaptor.getValue();
        assertThat(savedCard.getCardNumber()).isEqualTo(cardNumber);
        assertThat(savedCard.getAccountId()).isEqualTo(accountId);
        assertThat(savedCard.getCustomerId()).isEqualTo(customerId);
    }

    /**
     * Property Test: Card Update with Valid Data
     * For any valid card update data, the system should update the existing card
     * and persist changes to the database, returning the updated card details
     * Validates: Requirements 5.4
     */
    @Property(tries = 100)
    @Label("Feature: carddemo-modernization, Card Update with Valid Data")
    void testCardUpdateWithValidData(
            @ForAll("validCardNumbers") Long cardNumber,
            @ForAll("cardStatuses") String newStatus) {
        setupMocks();
        
        // Mock existing card
        Card existingCard = new Card();
        existingCard.setCardNumber(cardNumber);
        existingCard.setAccountId(12345678901L);
        existingCard.setCustomerId(123456789L);
        existingCard.setCardStatus("ACTIVE");
        existingCard.setExpirationDate("2025-12-31");
        existingCard.setIssueDate("2023-01-01");
        
        when(cardRepository.findByCardNumber(cardNumber)).thenReturn(Optional.of(existingCard));
        
        // Create update data
        CardUpdateDto updateData = new CardUpdateDto();
        updateData.setCardStatus(newStatus);
        updateData.setExpirationDate("2026-12-31");
        
        // Mock validation service
        ValidationResult validResult = ValidationResult.success();
        when(validationService.validateDate(anyString())).thenReturn(validResult);
        
        // Mock save operation
        ArgumentCaptor<Card> cardCaptor = ArgumentCaptor.forClass(Card.class);
        when(cardRepository.save(cardCaptor.capture())).thenAnswer(invocation -> invocation.getArgument(0));
        
        // Update card
        CardDto result = cardService.updateCard(cardNumber, updateData);
        
        // Verify card was updated correctly
        assertThat(result).isNotNull();
        assertThat(result.getCardNumber()).isEqualTo(cardNumber);
        assertThat(result.getCardStatus()).isEqualTo(newStatus);
        assertThat(result.getExpirationDate()).isEqualTo("2026-12-31");
        
        // Verify card was saved to repository
        Card savedCard = cardCaptor.getValue();
        assertThat(savedCard.getCardNumber()).isEqualTo(cardNumber);
        assertThat(savedCard.getCardStatus()).isEqualTo(newStatus);
        assertThat(savedCard.getExpirationDate()).isEqualTo("2026-12-31");
        
        // Original fields should remain unchanged
        assertThat(savedCard.getAccountId()).isEqualTo(12345678901L);
        assertThat(savedCard.getCustomerId()).isEqualTo(123456789L);
    }

    // Helper method to setup mocks for each test iteration
    private void setupMocks() {
        cardRepository = mock(CardRepository.class);
        accountRepository = mock(AccountRepository.class);
        customerRepository = mock(CustomerRepository.class);
        validationService = mock(ValidationService.class);
        
        cardService = new CardServiceImpl();
        
        // Use reflection to inject mocks
        try {
            java.lang.reflect.Field cardRepoField = CardServiceImpl.class.getDeclaredField("cardRepository");
            cardRepoField.setAccessible(true);
            cardRepoField.set(cardService, cardRepository);
            
            java.lang.reflect.Field accountRepoField = CardServiceImpl.class.getDeclaredField("accountRepository");
            accountRepoField.setAccessible(true);
            accountRepoField.set(cardService, accountRepository);
            
            java.lang.reflect.Field customerRepoField = CardServiceImpl.class.getDeclaredField("customerRepository");
            customerRepoField.setAccessible(true);
            customerRepoField.set(cardService, customerRepository);
            
            java.lang.reflect.Field validationServiceField = CardServiceImpl.class.getDeclaredField("validationService");
            validationServiceField.setAccessible(true);
            validationServiceField.set(cardService, validationService);
        } catch (Exception e) {
            throw new RuntimeException("Failed to inject mocks", e);
        }
    }

    // Providers for generating test data

    @Provide
    Arbitrary<Long> cardNumbers() {
        return Arbitraries.oneOf(
            // Valid 16-digit card numbers
            Arbitraries.longs().between(1000000000000000L, 9999999999999999L),
            // Invalid card numbers (too short)
            Arbitraries.longs().between(1L, 999999999999999L),
            // Invalid card numbers (too long - will be represented as 17+ digits)
            Arbitraries.longs().between(10000000000000000L, Long.MAX_VALUE)
        );
    }

    @Provide
    Arbitrary<Long> validCardNumbers() {
        // Generate valid 16-digit card numbers
        return Arbitraries.longs().between(1000000000000000L, 9999999999999999L);
    }

    @Provide
    Arbitrary<Long> accountIds() {
        // Generate valid 11-digit account IDs
        return Arbitraries.longs().between(10000000000L, 99999999999L);
    }

    @Provide
    Arbitrary<Long> customerIds() {
        // Generate valid 9-digit customer IDs
        return Arbitraries.longs().between(100000000L, 999999999L);
    }

    @Provide
    Arbitrary<String> cardStatuses() {
        return Arbitraries.of("ACTIVE", "INACTIVE", "BLOCKED", "EXPIRED", "PENDING");
    }
}
