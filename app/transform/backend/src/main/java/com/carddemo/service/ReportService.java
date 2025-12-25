package com.carddemo.service;

import com.carddemo.dto.AccountReportDto;
import com.carddemo.dto.CardReportDto;
import com.carddemo.dto.TransactionReportDto;

/**
 * Service interface for report generation
 * Provides methods to generate various reports on accounts, transactions, and cards
 */
public interface ReportService {

    /**
     * Generate account report with aggregated account data
     * @param status Optional filter by account status ('Y' for active, 'N' for inactive, null for all)
     * @param startDate Optional filter by open date (YYYY-MM-DD format)
     * @param endDate Optional filter by open date (YYYY-MM-DD format)
     * @return AccountReportDto containing aggregated account information
     */
    AccountReportDto generateAccountReport(String status, String startDate, String endDate);

    /**
     * Generate transaction report with aggregated transaction data
     * @param startDate Optional filter by transaction date (YYYY-MM-DD format)
     * @param endDate Optional filter by transaction date (YYYY-MM-DD format)
     * @param transactionType Optional filter by transaction type
     * @return TransactionReportDto containing aggregated transaction information
     */
    TransactionReportDto generateTransactionReport(String startDate, String endDate, String transactionType);

    /**
     * Generate card report with aggregated card data
     * @param status Optional filter by card status
     * @param expiringBefore Optional filter for cards expiring before this date (YYYY-MM-DD format)
     * @return CardReportDto containing aggregated card information
     */
    CardReportDto generateCardReport(String status, String expiringBefore);
}
