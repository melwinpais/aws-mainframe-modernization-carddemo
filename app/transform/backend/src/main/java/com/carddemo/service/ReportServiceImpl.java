package com.carddemo.service;

import com.carddemo.dto.AccountReportDto;
import com.carddemo.dto.CardReportDto;
import com.carddemo.dto.TransactionReportDto;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Transaction;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of ReportService
 * Generates various reports on accounts, transactions, and cards
 */
@Service
@Transactional(readOnly = true)
public class ReportServiceImpl implements ReportService {

    private static final Logger logger = LoggerFactory.getLogger(ReportServiceImpl.class);
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final CardRepository cardRepository;

    @Autowired
    public ReportServiceImpl(AccountRepository accountRepository,
                            TransactionRepository transactionRepository,
                            CardRepository cardRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
        this.cardRepository = cardRepository;
    }

    @Override
    public AccountReportDto generateAccountReport(String status, String startDate, String endDate) {
        logger.info("Generating account report with status: {}, startDate: {}, endDate: {}", 
                   status, startDate, endDate);

        // Retrieve all accounts
        List<Account> allAccounts = accountRepository.findAll();

        // Apply filters
        List<Account> filteredAccounts = allAccounts.stream()
            .filter(account -> status == null || account.getActiveStatus().equals(status))
            .filter(account -> startDate == null || isDateAfterOrEqual(account.getOpenDate(), startDate))
            .filter(account -> endDate == null || isDateBeforeOrEqual(account.getOpenDate(), endDate))
            .collect(Collectors.toList());

        // Calculate aggregates
        int totalAccounts = filteredAccounts.size();
        int activeAccounts = (int) filteredAccounts.stream()
            .filter(account -> "Y".equals(account.getActiveStatus()))
            .count();
        int inactiveAccounts = totalAccounts - activeAccounts;

        BigDecimal totalBalance = filteredAccounts.stream()
            .map(Account::getCurrentBalance)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal totalCreditLimit = filteredAccounts.stream()
            .map(Account::getCreditLimit)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal averageBalance = totalAccounts > 0 
            ? totalBalance.divide(BigDecimal.valueOf(totalAccounts), 2, RoundingMode.HALF_UP)
            : BigDecimal.ZERO;

        BigDecimal averageCreditLimit = totalAccounts > 0
            ? totalCreditLimit.divide(BigDecimal.valueOf(totalAccounts), 2, RoundingMode.HALF_UP)
            : BigDecimal.ZERO;

        // Convert to summary DTOs
        List<AccountReportDto.AccountSummaryDto> accountSummaries = filteredAccounts.stream()
            .map(account -> new AccountReportDto.AccountSummaryDto(
                account.getAccountId(),
                account.getActiveStatus(),
                account.getCurrentBalance(),
                account.getCreditLimit(),
                account.getOpenDate(),
                account.getExpirationDate()
            ))
            .collect(Collectors.toList());

        AccountReportDto report = new AccountReportDto(
            totalAccounts,
            activeAccounts,
            inactiveAccounts,
            totalBalance,
            totalCreditLimit,
            averageBalance,
            averageCreditLimit,
            accountSummaries
        );

        logger.info("Account report generated: {} total accounts, {} active, {} inactive",
                   totalAccounts, activeAccounts, inactiveAccounts);

        return report;
    }

    @Override
    public TransactionReportDto generateTransactionReport(String startDate, String endDate, String transactionType) {
        logger.info("Generating transaction report with startDate: {}, endDate: {}, type: {}",
                   startDate, endDate, transactionType);

        // Retrieve all transactions
        List<Transaction> allTransactions = transactionRepository.findAll();

        // Apply filters
        List<Transaction> filteredTransactions = allTransactions.stream()
            .filter(txn -> startDate == null || isDateAfterOrEqual(txn.getTransactionDate(), startDate))
            .filter(txn -> endDate == null || isDateBeforeOrEqual(txn.getTransactionDate(), endDate))
            .filter(txn -> transactionType == null || txn.getTransactionType().equals(transactionType))
            .collect(Collectors.toList());

        // Calculate aggregates
        int totalTransactions = filteredTransactions.size();

        BigDecimal totalAmount = filteredTransactions.stream()
            .map(Transaction::getAmount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal averageAmount = totalTransactions > 0
            ? totalAmount.divide(BigDecimal.valueOf(totalTransactions), 2, RoundingMode.HALF_UP)
            : BigDecimal.ZERO;

        BigDecimal maxAmount = filteredTransactions.stream()
            .map(Transaction::getAmount)
            .max(BigDecimal::compareTo)
            .orElse(BigDecimal.ZERO);

        BigDecimal minAmount = filteredTransactions.stream()
            .map(Transaction::getAmount)
            .min(BigDecimal::compareTo)
            .orElse(BigDecimal.ZERO);

        // Convert to summary DTOs
        List<TransactionReportDto.TransactionSummaryDto> transactionSummaries = filteredTransactions.stream()
            .map(txn -> new TransactionReportDto.TransactionSummaryDto(
                txn.getTransactionId(),
                txn.getAccountId(),
                txn.getCardNumber(),
                txn.getTransactionType(),
                txn.getAmount(),
                txn.getTransactionDate(),
                txn.getDescription()
            ))
            .collect(Collectors.toList());

        TransactionReportDto report = new TransactionReportDto(
            totalTransactions,
            totalAmount,
            averageAmount,
            maxAmount,
            minAmount,
            startDate,
            endDate,
            transactionSummaries
        );

        logger.info("Transaction report generated: {} total transactions, total amount: {}",
                   totalTransactions, totalAmount);

        return report;
    }

    @Override
    public CardReportDto generateCardReport(String status, String expiringBefore) {
        logger.info("Generating card report with status: {}, expiringBefore: {}",
                   status, expiringBefore);

        // Retrieve all cards
        List<Card> allCards = cardRepository.findAll();

        // Apply filters
        List<Card> filteredCards = allCards.stream()
            .filter(card -> status == null || card.getCardStatus().equals(status))
            .filter(card -> expiringBefore == null || isDateBeforeOrEqual(card.getExpirationDate(), expiringBefore))
            .collect(Collectors.toList());

        // Calculate aggregates
        int totalCards = filteredCards.size();
        int activeCards = (int) filteredCards.stream()
            .filter(card -> "ACTIVE".equalsIgnoreCase(card.getCardStatus()))
            .count();
        int inactiveCards = totalCards - activeCards;

        // Count expiring cards (within next 90 days)
        LocalDate today = LocalDate.now();
        LocalDate ninetyDaysFromNow = today.plusDays(90);
        int expiringCards = (int) filteredCards.stream()
            .filter(card -> {
                if (card.getExpirationDate() == null || card.getExpirationDate().isEmpty()) {
                    return false;
                }
                try {
                    LocalDate expDate = LocalDate.parse(card.getExpirationDate(), DATE_FORMATTER);
                    return !expDate.isBefore(today) && !expDate.isAfter(ninetyDaysFromNow);
                } catch (Exception e) {
                    return false;
                }
            })
            .count();

        // Convert to summary DTOs
        List<CardReportDto.CardSummaryDto> cardSummaries = filteredCards.stream()
            .map(card -> new CardReportDto.CardSummaryDto(
                card.getCardNumber(),
                card.getAccountId(),
                card.getCustomerId(),
                card.getCardStatus(),
                card.getExpirationDate(),
                card.getIssueDate()
            ))
            .collect(Collectors.toList());

        CardReportDto report = new CardReportDto(
            totalCards,
            activeCards,
            inactiveCards,
            expiringCards,
            cardSummaries
        );

        logger.info("Card report generated: {} total cards, {} active, {} inactive, {} expiring",
                   totalCards, activeCards, inactiveCards, expiringCards);

        return report;
    }

    /**
     * Helper method to check if a date is after or equal to another date
     */
    private boolean isDateAfterOrEqual(String date, String compareDate) {
        if (date == null || date.isEmpty() || compareDate == null || compareDate.isEmpty()) {
            return true;
        }
        try {
            LocalDate d1 = LocalDate.parse(date, DATE_FORMATTER);
            LocalDate d2 = LocalDate.parse(compareDate, DATE_FORMATTER);
            return !d1.isBefore(d2);
        } catch (Exception e) {
            logger.warn("Error parsing dates: {} and {}", date, compareDate, e);
            return true;
        }
    }

    /**
     * Helper method to check if a date is before or equal to another date
     */
    private boolean isDateBeforeOrEqual(String date, String compareDate) {
        if (date == null || date.isEmpty() || compareDate == null || compareDate.isEmpty()) {
            return true;
        }
        try {
            LocalDate d1 = LocalDate.parse(date, DATE_FORMATTER);
            LocalDate d2 = LocalDate.parse(compareDate, DATE_FORMATTER);
            return !d1.isAfter(d2);
        } catch (Exception e) {
            logger.warn("Error parsing dates: {} and {}", date, compareDate, e);
            return true;
        }
    }
}
