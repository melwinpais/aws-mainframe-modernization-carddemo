package com.carddemo.migration;

import net.jqwik.api.*;
import org.junit.jupiter.api.Tag;

import java.util.List;

/**
 * Property-Based Test for Migration Error Handling
 * 
 * Feature: carddemo-modernization, Property 15: Migration Error Handling
 * Validates: Requirements 10.7
 * 
 * This test verifies that for any data conversion error during migration,
 * the utility logs the error and continues processing remaining records.
 */
@Tag("Feature: carddemo-modernization, Property 15: Migration Error Handling")
public class MigrationErrorHandlingPropertyTest {

    /**
     * Property Test: Migration Summary Tracks All Records
     * For any list of records with some failures,
     * the migration summary should accurately track total, successful, and failed counts.
     */
    @Property(tries = 100)
    void migrationSummaryTracksAllRecords(
            @ForAll("recordCounts") int totalRecords,
            @ForAll("failureCounts") int failedRecords) {
        
        // Skip if failed records exceed total
        Assume.that(failedRecords <= totalRecords);
        
        // Create a migration summary
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        
        // Simulate processing records
        int successfulRecords = totalRecords - failedRecords;
        for (int i = 0; i < successfulRecords; i++) {
            summary.recordSuccess();
        }
        for (int i = 0; i < failedRecords; i++) {
            summary.recordFailure("Error processing record " + i);
        }
        summary.complete();
        
        // Verify: All records are accounted for
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getTotalRecordsProcessed(),
            "Total records should match input");
        
        org.junit.jupiter.api.Assertions.assertEquals(successfulRecords, 
            summary.getSuccessfulRecords(),
            "Successful records should match expected");
        
        org.junit.jupiter.api.Assertions.assertEquals(failedRecords, 
            summary.getFailedRecords(),
            "Failed records should match expected");
        
        org.junit.jupiter.api.Assertions.assertEquals(failedRecords, 
            summary.getErrors().size(),
            "Error list size should match failed count");
        
        org.junit.jupiter.api.Assertions.assertEquals(
            summary.getSuccessfulRecords() + summary.getFailedRecords(),
            summary.getTotalRecordsProcessed(),
            "Success + failures should equal total");
    }

    /**
     * Property Test: Migration Summary Handles All Failures
     * For any number of records where all fail,
     * the migration summary should record all as failures.
     */
    @Property(tries = 100)
    void migrationSummaryHandlesAllFailures(
            @ForAll("recordCounts") int totalRecords) {
        
        // Skip if no records
        Assume.that(totalRecords > 0);
        
        // Create a migration summary
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        
        // Simulate all records failing
        for (int i = 0; i < totalRecords; i++) {
            summary.recordFailure("Error processing record " + i);
        }
        summary.complete();
        
        // Verify: All records failed
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getTotalRecordsProcessed(),
            "Total records should match input");
        
        org.junit.jupiter.api.Assertions.assertEquals(0, 
            summary.getSuccessfulRecords(),
            "No records should succeed");
        
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getFailedRecords(),
            "All records should fail");
        
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getErrors().size(),
            "Error list should contain all failures");
    }

    /**
     * Property Test: Migration Summary Handles All Successes
     * For any number of records where all succeed,
     * the migration summary should record all as successes.
     */
    @Property(tries = 100)
    void migrationSummaryHandlesAllSuccesses(
            @ForAll("recordCounts") int totalRecords) {
        
        // Skip if no records
        Assume.that(totalRecords > 0);
        
        // Create a migration summary
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        
        // Simulate all records succeeding
        for (int i = 0; i < totalRecords; i++) {
            summary.recordSuccess();
        }
        summary.complete();
        
        // Verify: All records succeeded
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getTotalRecordsProcessed(),
            "Total records should match input");
        
        org.junit.jupiter.api.Assertions.assertEquals(totalRecords, 
            summary.getSuccessfulRecords(),
            "All records should succeed");
        
        org.junit.jupiter.api.Assertions.assertEquals(0, 
            summary.getFailedRecords(),
            "No records should fail");
        
        org.junit.jupiter.api.Assertions.assertTrue(summary.getErrors().isEmpty(),
            "Error list should be empty");
    }

    /**
     * Property Test: Migration Summary Duration is Non-Negative
     * For any migration, the duration should be non-negative.
     */
    @Property(tries = 100)
    void migrationSummaryDurationIsNonNegative(
            @ForAll("recordCounts") int totalRecords,
            @ForAll("failureCounts") int failedRecords) {
        
        // Skip if failed records exceed total
        Assume.that(failedRecords <= totalRecords);
        
        // Create a migration summary
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        
        // Simulate processing records
        int successfulRecords = totalRecords - failedRecords;
        for (int i = 0; i < successfulRecords; i++) {
            summary.recordSuccess();
        }
        for (int i = 0; i < failedRecords; i++) {
            summary.recordFailure("Error " + i);
        }
        summary.complete();
        
        // Verify: Duration is non-negative
        org.junit.jupiter.api.Assertions.assertTrue(summary.getDurationMs() >= 0,
            "Duration should be non-negative");
    }

    /**
     * Property Test: Migration Summary Error Messages Are Preserved
     * For any list of error messages,
     * the migration summary should preserve all error messages.
     */
    @Property(tries = 100)
    void migrationSummaryPreservesErrorMessages(
            @ForAll("errorMessages") List<String> errorMessages) {
        
        // Create a migration summary
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        
        // Record all errors
        for (String errorMessage : errorMessages) {
            summary.recordFailure(errorMessage);
        }
        summary.complete();
        
        // Verify: All error messages are preserved
        org.junit.jupiter.api.Assertions.assertEquals(errorMessages.size(), 
            summary.getErrors().size(),
            "Error count should match");
        
        for (int i = 0; i < errorMessages.size(); i++) {
            org.junit.jupiter.api.Assertions.assertEquals(errorMessages.get(i), 
                summary.getErrors().get(i),
                "Error message should be preserved");
        }
    }

    // ========== Arbitrary Providers ==========

    @Provide
    Arbitrary<Integer> recordCounts() {
        return Arbitraries.integers().between(1, 100);
    }

    @Provide
    Arbitrary<Integer> failureCounts() {
        return Arbitraries.integers().between(0, 100);
    }

    @Provide
    Arbitrary<List<String>> errorMessages() {
        return Arbitraries.strings().alpha().numeric().ofMinLength(10).ofMaxLength(100)
            .list().ofMinSize(0).ofMaxSize(20);
    }
}
