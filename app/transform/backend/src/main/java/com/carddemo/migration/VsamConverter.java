package com.carddemo.migration;

import com.carddemo.entity.*;
import com.carddemo.migration.model.*;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * Converter for transforming VSAM records to PostgreSQL entities.
 * Handles conversion of all record types: User, Account, Customer, Card, and Transaction.
 */
@Component
public class VsamConverter {

    /**
     * Convert VSAM User record (USRSEC) to PostgreSQL User entity.
     * 
     * @param vsamRecord VSAM user record
     * @return User entity
     */
    public User convertUserRecord(VsamUserRecord vsamRecord) {
        if (vsamRecord == null) {
            throw new IllegalArgumentException("VSAM user record cannot be null");
        }

        User user = new User();
        user.setUserId(vsamRecord.getUserId());
        user.setFirstName(vsamRecord.getFirstName());
        user.setLastName(vsamRecord.getLastName());
        user.setPassword(vsamRecord.getPassword());
        user.setUserType(vsamRecord.getUserType());
        
        return user;
    }

    /**
     * Convert VSAM Account record (ACCTDAT) to PostgreSQL Account entity.
     * 
     * @param vsamRecord VSAM account record
     * @return Account entity
     */
    public Account convertAccountRecord(VsamAccountRecord vsamRecord) {
        if (vsamRecord == null) {
            throw new IllegalArgumentException("VSAM account record cannot be null");
        }

        Account account = new Account();
        account.setAccountId(vsamRecord.getAccountId());
        account.setActiveStatus(vsamRecord.getActiveStatus());
        account.setCurrentBalance(vsamRecord.getCurrentBalance());
        account.setCreditLimit(vsamRecord.getCreditLimit());
        account.setCashCreditLimit(vsamRecord.getCashCreditLimit());
        account.setOpenDate(vsamRecord.getOpenDate());
        account.setExpirationDate(vsamRecord.getExpirationDate());
        account.setReissueDate(vsamRecord.getReissueDate());
        account.setCurrentCycleCredit(vsamRecord.getCurrentCycleCredit());
        account.setCurrentCycleDebit(vsamRecord.getCurrentCycleDebit());
        account.setAddressZip(vsamRecord.getAddressZip());
        account.setGroupId(vsamRecord.getGroupId());
        
        return account;
    }

    /**
     * Convert VSAM Customer record (CUSTDAT) to PostgreSQL Customer entity.
     * 
     * @param vsamRecord VSAM customer record
     * @return Customer entity
     */
    public Customer convertCustomerRecord(VsamCustomerRecord vsamRecord) {
        if (vsamRecord == null) {
            throw new IllegalArgumentException("VSAM customer record cannot be null");
        }

        Customer customer = new Customer();
        customer.setCustomerId(vsamRecord.getCustomerId());
        customer.setFirstName(vsamRecord.getFirstName());
        customer.setMiddleName(vsamRecord.getMiddleName());
        customer.setLastName(vsamRecord.getLastName());
        customer.setAddressLine1(vsamRecord.getAddressLine1());
        customer.setAddressLine2(vsamRecord.getAddressLine2());
        customer.setAddressLine3(vsamRecord.getAddressLine3());
        customer.setStateCode(vsamRecord.getStateCode());
        customer.setCountryCode(vsamRecord.getCountryCode());
        customer.setZipCode(vsamRecord.getZipCode());
        customer.setPhoneNumber1(vsamRecord.getPhoneNumber1());
        customer.setPhoneNumber2(vsamRecord.getPhoneNumber2());
        customer.setSsn(vsamRecord.getSsn());
        customer.setGovernmentIssuedId(vsamRecord.getGovernmentIssuedId());
        customer.setDateOfBirth(vsamRecord.getDateOfBirth());
        customer.setEftAccountId(vsamRecord.getEftAccountId());
        customer.setPrimaryCardholderInd(vsamRecord.getPrimaryCardholderInd());
        customer.setFicoCreditScore(vsamRecord.getFicoCreditScore());
        
        return customer;
    }

    /**
     * Convert VSAM Card record (CARDDAT) to PostgreSQL Card entity.
     * 
     * @param vsamRecord VSAM card record
     * @return Card entity
     */
    public Card convertCardRecord(VsamCardRecord vsamRecord) {
        if (vsamRecord == null) {
            throw new IllegalArgumentException("VSAM card record cannot be null");
        }

        Card card = new Card();
        card.setCardNumber(vsamRecord.getCardNumber());
        card.setAccountId(vsamRecord.getAccountId());
        card.setCustomerId(vsamRecord.getCustomerId());
        card.setCardStatus(vsamRecord.getCardStatus());
        card.setExpirationDate(vsamRecord.getExpirationDate());
        card.setIssueDate(vsamRecord.getIssueDate());
        
        return card;
    }

    /**
     * Convert VSAM Transaction record (TRANSACT) to PostgreSQL Transaction entity.
     * 
     * @param vsamRecord VSAM transaction record
     * @return Transaction entity
     */
    public Transaction convertTransactionRecord(VsamTransactionRecord vsamRecord) {
        if (vsamRecord == null) {
            throw new IllegalArgumentException("VSAM transaction record cannot be null");
        }

        Transaction transaction = new Transaction();
        // Note: transactionId is auto-generated in PostgreSQL, so we don't set it from VSAM
        transaction.setAccountId(vsamRecord.getAccountId());
        transaction.setCardNumber(vsamRecord.getCardNumber());
        transaction.setTransactionType(vsamRecord.getTransactionType());
        transaction.setTransactionCategory(vsamRecord.getTransactionCategory());
        transaction.setAmount(vsamRecord.getAmount());
        transaction.setTransactionDate(vsamRecord.getTransactionDate());
        transaction.setTransactionTime(vsamRecord.getTransactionTime());
        transaction.setDescription(vsamRecord.getDescription());
        transaction.setMerchantName(vsamRecord.getMerchantName());
        transaction.setMerchantCity(vsamRecord.getMerchantCity());
        transaction.setMerchantZip(vsamRecord.getMerchantZip());
        
        return transaction;
    }
}
