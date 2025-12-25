package com.carddemo.migration;

import com.carddemo.model.*;
import com.carddemo.util.VsamPostgresConverter;
import net.jqwik.api.*;
import net.jqwik.api.constraints.*;
import org.junit.jupiter.api.Tag;

import java.math.BigDecimal;

/**
 * Property-Based Test for VSAM to PostgreSQL Conversion Round Trip
 * 
 * Feature: carddemo-modernization, Property 14: VSAM to PostgreSQL Conversion Round Trip
 * Validates: Requirements 10.2, 10.3, 10.4, 10.5, 10.6
 * 
 * This test verifies that for any valid VSAM record (USRSEC, ACCTDAT, CUSTDAT, CARDDAT, or TRANSACT),
 * converting to PostgreSQL format and then reading back produces an equivalent record with all field values preserved.
 */
@Tag("Feature: carddemo-modernization, Property 14: VSAM to PostgreSQL Conversion Round Trip")
public class VsamToPostgresRoundTripPropertyTest {

    /**
     * Property Test: Account Record Round Trip
     * For any valid VSAM account record, converting to PostgreSQL and back should preserve all values.
     */
    @Property(tries = 100)
    void accountRecordRoundTripPreservesAllFields(
            @ForAll("validAccountRecords") VsamAccountRecord original) {
        
        // Perform round trip conversion
        VsamAccountRecord converted = VsamPostgresConverter.accountRoundTrip(original);
        
        // Verify all fields are preserved
        org.junit.jupiter.api.Assertions.assertEquals(original, converted,
            "Account record should be identical after round trip conversion");
    }

    /**
     * Property Test: Customer Record Round Trip
     * For any valid VSAM customer record, converting to PostgreSQL and back should preserve all values.
     */
    @Property(tries = 100)
    void customerRecordRoundTripPreservesAllFields(
            @ForAll("validCustomerRecords") VsamCustomerRecord original) {
        
        // Perform round trip conversion
        VsamCustomerRecord converted = VsamPostgresConverter.customerRoundTrip(original);
        
        // Verify all fields are preserved
        org.junit.jupiter.api.Assertions.assertEquals(original, converted,
            "Customer record should be identical after round trip conversion");
    }

    /**
     * Property Test: Card Record Round Trip
     * For any valid VSAM card record, converting to PostgreSQL and back should preserve all values.
     */
    @Property(tries = 100)
    void cardRecordRoundTripPreservesAllFields(
            @ForAll("validCardRecords") VsamCardRecord original) {
        
        // Perform round trip conversion
        VsamCardRecord converted = VsamPostgresConverter.cardRoundTrip(original);
        
        // Verify all fields are preserved
        org.junit.jupiter.api.Assertions.assertEquals(original, converted,
            "Card record should be identical after round trip conversion");
    }

    /**
     * Property Test: User Record Round Trip
     * For any valid VSAM user record, converting to PostgreSQL and back should preserve all values.
     */
    @Property(tries = 100)
    void userRecordRoundTripPreservesAllFields(
            @ForAll("validUserRecords") VsamUserRecord original) {
        
        // Perform round trip conversion
        VsamUserRecord converted = VsamPostgresConverter.userRoundTrip(original);
        
        // Verify all fields are preserved
        org.junit.jupiter.api.Assertions.assertEquals(original, converted,
            "User record should be identical after round trip conversion");
    }

    /**
     * Property Test: Transaction Record Round Trip
     * For any valid VSAM transaction record, converting to PostgreSQL and back should preserve all values.
     */
    @Property(tries = 100)
    void transactionRecordRoundTripPreservesAllFields(
            @ForAll("validTransactionRecords") VsamTransactionRecord original) {
        
        // Perform round trip conversion
        VsamTransactionRecord converted = VsamPostgresConverter.transactionRoundTrip(original);
        
        // Verify all fields are preserved
        org.junit.jupiter.api.Assertions.assertEquals(original, converted,
            "Transaction record should be identical after round trip conversion");
    }

    // ========== Arbitrary Providers ==========

    /**
     * Generates valid VSAM account records matching CVACT01Y.cpy structure
     */
    @Provide
    Arbitrary<VsamAccountRecord> validAccountRecords() {
        Arbitrary<Long> accountIds = Arbitraries.longs().between(10000000000L, 99999999999L);
        Arbitrary<String> statuses = Arbitraries.of("Y", "N");
        Arbitrary<BigDecimal> balances = Arbitraries.bigDecimals()
            .between(BigDecimal.valueOf(-9999999999.99), BigDecimal.valueOf(9999999999.99))
            .ofScale(2);
        Arbitrary<BigDecimal> limits = Arbitraries.bigDecimals()
            .between(BigDecimal.ZERO, BigDecimal.valueOf(9999999999.99))
            .ofScale(2);
        Arbitrary<String> dates = validDates();
        Arbitrary<String> zips = Arbitraries.strings().alpha().numeric().ofMaxLength(10);
        
        return Combinators.combine(accountIds, statuses, balances, limits, limits, dates, dates, dates)
            .as((id, status, balance, creditLimit, cashLimit, openDate, expDate, reissueDate) -> {
                VsamAccountRecord record = new VsamAccountRecord();
                record.setAccountId(id);
                record.setActiveStatus(status);
                record.setCurrentBalance(balance);
                record.setCreditLimit(creditLimit);
                record.setCashCreditLimit(cashLimit);
                record.setOpenDate(openDate);
                record.setExpirationDate(expDate);
                record.setReissueDate(reissueDate);
                return record;
            })
            .flatMap(record -> Combinators.combine(limits, limits, zips, zips)
                .as((cycleCredit, cycleDebit, zip, groupId) -> {
                    record.setCurrentCycleCredit(cycleCredit);
                    record.setCurrentCycleDebit(cycleDebit);
                    record.setAddressZip(zip);
                    record.setGroupId(groupId);
                    return record;
                }));
    }

    /**
     * Generates valid VSAM customer records matching CVCUS01Y.cpy structure
     */
    @Provide
    Arbitrary<VsamCustomerRecord> validCustomerRecords() {
        Arbitrary<Long> customerIds = Arbitraries.longs().between(100000000L, 999999999L);
        Arbitrary<String> names = Arbitraries.strings().alpha().ofMaxLength(25);
        Arbitrary<String> addresses = Arbitraries.strings().ofMaxLength(50);
        Arbitrary<String> stateCodes = Arbitraries.strings().alpha().ofLength(2);
        Arbitrary<String> countryCodes = Arbitraries.strings().alpha().ofLength(3);
        Arbitrary<String> zips = Arbitraries.strings().numeric().ofMaxLength(10);
        Arbitrary<String> phones = Arbitraries.strings().numeric().ofMaxLength(15);
        Arbitrary<Long> ssns = Arbitraries.longs().between(100000000L, 999999999L);
        
        return Combinators.combine(customerIds, names, names, names, addresses, addresses, addresses, stateCodes)
            .as((id, fname, mname, lname, addr1, addr2, addr3, state) -> {
                VsamCustomerRecord record = new VsamCustomerRecord();
                record.setCustomerId(id);
                record.setFirstName(fname);
                record.setMiddleName(mname);
                record.setLastName(lname);
                record.setAddressLine1(addr1);
                record.setAddressLine2(addr2);
                record.setAddressLine3(addr3);
                record.setStateCode(state);
                return record;
            })
            .flatMap(record -> Combinators.combine(countryCodes, zips, phones, phones, ssns, 
                    Arbitraries.strings().ofMaxLength(20), validDates(), Arbitraries.strings().ofMaxLength(10))
                .as((country, zip, phone1, phone2, ssn, govId, dob, eft) -> {
                    record.setCountryCode(country);
                    record.setZipCode(zip);
                    record.setPhoneNumber1(phone1);
                    record.setPhoneNumber2(phone2);
                    record.setSsn(ssn);
                    record.setGovernmentIssuedId(govId);
                    record.setDateOfBirth(dob);
                    record.setEftAccountId(eft);
                    return record;
                }))
            .flatMap(record -> Combinators.combine(Arbitraries.of("Y", "N"), Arbitraries.integers().between(300, 850))
                .as((indicator, fico) -> {
                    record.setPrimaryCardholderInd(indicator);
                    record.setFicoCreditScore(fico);
                    return record;
                }));
    }

    /**
     * Generates valid VSAM card records matching CVACT02Y.cpy structure
     */
    @Provide
    Arbitrary<VsamCardRecord> validCardRecords() {
        return Combinators.combine(
            Arbitraries.strings().numeric().ofLength(16),               // Card number (16 digits)
            Arbitraries.longs().between(10000000000L, 99999999999L),   // Account ID (11 digits)
            Arbitraries.integers().between(100, 999),                   // CVV code (3 digits)
            Arbitraries.strings().alpha().withChars(' ').ofMaxLength(50), // Embossed name
            validDates(),                                                // Expiration date
            Arbitraries.of("Y", "N")                                    // Active status
        ).as(VsamCardRecord::new);
    }

    /**
     * Generates valid VSAM user records matching CSUSR01Y.cpy structure
     */
    @Provide
    Arbitrary<VsamUserRecord> validUserRecords() {
        return Combinators.combine(
            Arbitraries.strings().alpha().numeric().ofLength(8),       // User ID (8 chars)
            Arbitraries.strings().alpha().ofMaxLength(20),              // First name
            Arbitraries.strings().alpha().ofMaxLength(20),              // Last name
            Arbitraries.strings().ofLength(8),                          // Password (8 chars)
            Arbitraries.of("A", "U")                                    // User type (Admin or User)
        ).as(VsamUserRecord::new);
    }

    /**
     * Generates valid VSAM transaction records matching CVTRA05Y.cpy structure
     */
    @Provide
    Arbitrary<VsamTransactionRecord> validTransactionRecords() {
        Arbitrary<String> txnIds = Arbitraries.strings().numeric().ofLength(16);
        Arbitrary<String> typeCodes = Arbitraries.strings().alpha().numeric().ofLength(2);
        Arbitrary<Integer> catCodes = Arbitraries.integers().between(1000, 9999);
        Arbitrary<String> sources = Arbitraries.strings().ofMaxLength(10);
        Arbitrary<String> descriptions = Arbitraries.strings().ofMaxLength(100);
        Arbitrary<BigDecimal> amounts = Arbitraries.bigDecimals()
            .between(BigDecimal.valueOf(-999999999.99), BigDecimal.valueOf(999999999.99))
            .ofScale(2);
        Arbitrary<Long> merchantIds = Arbitraries.longs().between(100000000L, 999999999L);
        Arbitrary<String> merchantNames = Arbitraries.strings().ofMaxLength(50);
        
        return Combinators.combine(txnIds, typeCodes, catCodes, sources, descriptions, amounts, merchantIds, merchantNames)
            .as((id, typeCode, catCode, source, desc, amount, merchId, merchName) -> {
                VsamTransactionRecord record = new VsamTransactionRecord();
                record.setTransactionId(id);
                record.setTransactionTypeCode(typeCode);
                record.setTransactionCatCode(catCode);
                record.setTransactionSource(source);
                record.setDescription(desc);
                record.setAmount(amount);
                record.setMerchantId(merchId);
                record.setMerchantName(merchName);
                return record;
            })
            .flatMap(record -> Combinators.combine(
                    Arbitraries.strings().ofMaxLength(50),
                    Arbitraries.strings().numeric().ofMaxLength(10),
                    Arbitraries.strings().numeric().ofLength(16),
                    validTimestamps(),
                    validTimestamps())
                .as((city, zip, cardNum, origTs, procTs) -> {
                    record.setMerchantCity(city);
                    record.setMerchantZip(zip);
                    record.setCardNumber(cardNum);
                    record.setOriginalTimestamp(origTs);
                    record.setProcessedTimestamp(procTs);
                    return record;
                }));
    }

    /**
     * Generates valid dates in YYYY-MM-DD format
     */
    @Provide
    Arbitrary<String> validDates() {
        return Combinators.combine(
            Arbitraries.integers().between(2000, 2030),                 // Year
            Arbitraries.integers().between(1, 12),                      // Month
            Arbitraries.integers().between(1, 28)                       // Day (simplified to avoid invalid dates)
        ).as((year, month, day) -> 
            String.format("%04d-%02d-%02d", year, month, day));
    }

    /**
     * Generates valid timestamps in ISO format
     */
    @Provide
    Arbitrary<String> validTimestamps() {
        return Combinators.combine(
            validDates(),
            Arbitraries.integers().between(0, 23),                      // Hour
            Arbitraries.integers().between(0, 59),                      // Minute
            Arbitraries.integers().between(0, 59)                       // Second
        ).as((date, hour, minute, second) -> 
            String.format("%sT%02d:%02d:%02d.000Z", date, hour, minute, second));
    }
}
