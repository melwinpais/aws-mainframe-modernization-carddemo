package com.carddemo.util;

import com.carddemo.model.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Utility class to simulate VSAM to PostgreSQL conversion and back.
 * This simulates the data migration process for testing purposes.
 */
public class VsamPostgresConverter {

    /**
     * Simulates converting VSAM account record to PostgreSQL format and back.
     * In a real implementation, this would write to database and read back.
     * For testing, we use an in-memory map to simulate the round trip.
     */
    public static VsamAccountRecord accountRoundTrip(VsamAccountRecord original) {
        // Simulate conversion to PostgreSQL format (stored in map)
        Map<String, Object> pgFormat = new HashMap<>();
        pgFormat.put("account_id", original.getAccountId());
        pgFormat.put("active_status", original.getActiveStatus());
        pgFormat.put("current_balance", original.getCurrentBalance());
        pgFormat.put("credit_limit", original.getCreditLimit());
        pgFormat.put("cash_credit_limit", original.getCashCreditLimit());
        pgFormat.put("open_date", original.getOpenDate());
        pgFormat.put("expiration_date", original.getExpirationDate());
        pgFormat.put("reissue_date", original.getReissueDate());
        pgFormat.put("current_cycle_credit", original.getCurrentCycleCredit());
        pgFormat.put("current_cycle_debit", original.getCurrentCycleDebit());
        pgFormat.put("address_zip", original.getAddressZip());
        pgFormat.put("group_id", original.getGroupId());

        // Simulate reading back from PostgreSQL and converting to VSAM format
        VsamAccountRecord converted = new VsamAccountRecord();
        converted.setAccountId((Long) pgFormat.get("account_id"));
        converted.setActiveStatus((String) pgFormat.get("active_status"));
        converted.setCurrentBalance((java.math.BigDecimal) pgFormat.get("current_balance"));
        converted.setCreditLimit((java.math.BigDecimal) pgFormat.get("credit_limit"));
        converted.setCashCreditLimit((java.math.BigDecimal) pgFormat.get("cash_credit_limit"));
        converted.setOpenDate((String) pgFormat.get("open_date"));
        converted.setExpirationDate((String) pgFormat.get("expiration_date"));
        converted.setReissueDate((String) pgFormat.get("reissue_date"));
        converted.setCurrentCycleCredit((java.math.BigDecimal) pgFormat.get("current_cycle_credit"));
        converted.setCurrentCycleDebit((java.math.BigDecimal) pgFormat.get("current_cycle_debit"));
        converted.setAddressZip((String) pgFormat.get("address_zip"));
        converted.setGroupId((String) pgFormat.get("group_id"));

        return converted;
    }

    /**
     * Simulates converting VSAM customer record to PostgreSQL format and back.
     */
    public static VsamCustomerRecord customerRoundTrip(VsamCustomerRecord original) {
        Map<String, Object> pgFormat = new HashMap<>();
        pgFormat.put("customer_id", original.getCustomerId());
        pgFormat.put("first_name", original.getFirstName());
        pgFormat.put("middle_name", original.getMiddleName());
        pgFormat.put("last_name", original.getLastName());
        pgFormat.put("address_line_1", original.getAddressLine1());
        pgFormat.put("address_line_2", original.getAddressLine2());
        pgFormat.put("address_line_3", original.getAddressLine3());
        pgFormat.put("state_code", original.getStateCode());
        pgFormat.put("country_code", original.getCountryCode());
        pgFormat.put("zip_code", original.getZipCode());
        pgFormat.put("phone_number_1", original.getPhoneNumber1());
        pgFormat.put("phone_number_2", original.getPhoneNumber2());
        pgFormat.put("ssn", original.getSsn());
        pgFormat.put("government_issued_id", original.getGovernmentIssuedId());
        pgFormat.put("date_of_birth", original.getDateOfBirth());
        pgFormat.put("eft_account_id", original.getEftAccountId());
        pgFormat.put("primary_cardholder_ind", original.getPrimaryCardholderInd());
        pgFormat.put("fico_credit_score", original.getFicoCreditScore());

        VsamCustomerRecord converted = new VsamCustomerRecord();
        converted.setCustomerId((Long) pgFormat.get("customer_id"));
        converted.setFirstName((String) pgFormat.get("first_name"));
        converted.setMiddleName((String) pgFormat.get("middle_name"));
        converted.setLastName((String) pgFormat.get("last_name"));
        converted.setAddressLine1((String) pgFormat.get("address_line_1"));
        converted.setAddressLine2((String) pgFormat.get("address_line_2"));
        converted.setAddressLine3((String) pgFormat.get("address_line_3"));
        converted.setStateCode((String) pgFormat.get("state_code"));
        converted.setCountryCode((String) pgFormat.get("country_code"));
        converted.setZipCode((String) pgFormat.get("zip_code"));
        converted.setPhoneNumber1((String) pgFormat.get("phone_number_1"));
        converted.setPhoneNumber2((String) pgFormat.get("phone_number_2"));
        converted.setSsn((Long) pgFormat.get("ssn"));
        converted.setGovernmentIssuedId((String) pgFormat.get("government_issued_id"));
        converted.setDateOfBirth((String) pgFormat.get("date_of_birth"));
        converted.setEftAccountId((String) pgFormat.get("eft_account_id"));
        converted.setPrimaryCardholderInd((String) pgFormat.get("primary_cardholder_ind"));
        converted.setFicoCreditScore((Integer) pgFormat.get("fico_credit_score"));

        return converted;
    }

    /**
     * Simulates converting VSAM card record to PostgreSQL format and back.
     */
    public static VsamCardRecord cardRoundTrip(VsamCardRecord original) {
        Map<String, Object> pgFormat = new HashMap<>();
        pgFormat.put("card_number", original.getCardNumber());
        pgFormat.put("account_id", original.getAccountId());
        pgFormat.put("cvv_code", original.getCvvCode());
        pgFormat.put("embossed_name", original.getEmbossedName());
        pgFormat.put("expiration_date", original.getExpirationDate());
        pgFormat.put("active_status", original.getActiveStatus());

        VsamCardRecord converted = new VsamCardRecord();
        converted.setCardNumber((String) pgFormat.get("card_number"));
        converted.setAccountId((Long) pgFormat.get("account_id"));
        converted.setCvvCode((Integer) pgFormat.get("cvv_code"));
        converted.setEmbossedName((String) pgFormat.get("embossed_name"));
        converted.setExpirationDate((String) pgFormat.get("expiration_date"));
        converted.setActiveStatus((String) pgFormat.get("active_status"));

        return converted;
    }

    /**
     * Simulates converting VSAM user record to PostgreSQL format and back.
     */
    public static VsamUserRecord userRoundTrip(VsamUserRecord original) {
        Map<String, Object> pgFormat = new HashMap<>();
        pgFormat.put("user_id", original.getUserId());
        pgFormat.put("first_name", original.getFirstName());
        pgFormat.put("last_name", original.getLastName());
        pgFormat.put("password", original.getPassword());
        pgFormat.put("user_type", original.getUserType());

        VsamUserRecord converted = new VsamUserRecord();
        converted.setUserId((String) pgFormat.get("user_id"));
        converted.setFirstName((String) pgFormat.get("first_name"));
        converted.setLastName((String) pgFormat.get("last_name"));
        converted.setPassword((String) pgFormat.get("password"));
        converted.setUserType((String) pgFormat.get("user_type"));

        return converted;
    }

    /**
     * Simulates converting VSAM transaction record to PostgreSQL format and back.
     */
    public static VsamTransactionRecord transactionRoundTrip(VsamTransactionRecord original) {
        Map<String, Object> pgFormat = new HashMap<>();
        pgFormat.put("transaction_id", original.getTransactionId());
        pgFormat.put("transaction_type_code", original.getTransactionTypeCode());
        pgFormat.put("transaction_cat_code", original.getTransactionCatCode());
        pgFormat.put("transaction_source", original.getTransactionSource());
        pgFormat.put("description", original.getDescription());
        pgFormat.put("amount", original.getAmount());
        pgFormat.put("merchant_id", original.getMerchantId());
        pgFormat.put("merchant_name", original.getMerchantName());
        pgFormat.put("merchant_city", original.getMerchantCity());
        pgFormat.put("merchant_zip", original.getMerchantZip());
        pgFormat.put("card_number", original.getCardNumber());
        pgFormat.put("original_timestamp", original.getOriginalTimestamp());
        pgFormat.put("processed_timestamp", original.getProcessedTimestamp());

        VsamTransactionRecord converted = new VsamTransactionRecord();
        converted.setTransactionId((String) pgFormat.get("transaction_id"));
        converted.setTransactionTypeCode((String) pgFormat.get("transaction_type_code"));
        converted.setTransactionCatCode((Integer) pgFormat.get("transaction_cat_code"));
        converted.setTransactionSource((String) pgFormat.get("transaction_source"));
        converted.setDescription((String) pgFormat.get("description"));
        converted.setAmount((java.math.BigDecimal) pgFormat.get("amount"));
        converted.setMerchantId((Long) pgFormat.get("merchant_id"));
        converted.setMerchantName((String) pgFormat.get("merchant_name"));
        converted.setMerchantCity((String) pgFormat.get("merchant_city"));
        converted.setMerchantZip((String) pgFormat.get("merchant_zip"));
        converted.setCardNumber((String) pgFormat.get("card_number"));
        converted.setOriginalTimestamp((String) pgFormat.get("original_timestamp"));
        converted.setProcessedTimestamp((String) pgFormat.get("processed_timestamp"));

        return converted;
    }
}
