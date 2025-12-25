package com.carddemo.controller;

import com.carddemo.dto.AccountReportDto;
import com.carddemo.dto.CardReportDto;
import com.carddemo.dto.TransactionReportDto;
import com.carddemo.service.ReportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST Controller for report generation
 * Provides endpoints to generate various reports on accounts, transactions, and cards
 */
@RestController
@RequestMapping("/api/reports")
@CrossOrigin(origins = "*")
public class ReportController {

    private static final Logger logger = LoggerFactory.getLogger(ReportController.class);

    private final ReportService reportService;

    @Autowired
    public ReportController(ReportService reportService) {
        this.reportService = reportService;
    }

    /**
     * Generate account report
     * GET /api/reports/accounts
     * 
     * @param status Optional filter by account status ('Y' for active, 'N' for inactive)
     * @param startDate Optional filter by open date (YYYY-MM-DD format)
     * @param endDate Optional filter by open date (YYYY-MM-DD format)
     * @return AccountReportDto containing aggregated account information
     */
    @GetMapping("/accounts")
    public ResponseEntity<AccountReportDto> generateAccountReport(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate) {
        
        logger.info("Received request to generate account report - status: {}, startDate: {}, endDate: {}",
                   status, startDate, endDate);

        AccountReportDto report = reportService.generateAccountReport(status, startDate, endDate);

        logger.info("Account report generated successfully with {} accounts", 
                   report.getTotalAccounts());

        return ResponseEntity.ok(report);
    }

    /**
     * Generate transaction report
     * GET /api/reports/transactions
     * 
     * @param startDate Optional filter by transaction date (YYYY-MM-DD format)
     * @param endDate Optional filter by transaction date (YYYY-MM-DD format)
     * @param type Optional filter by transaction type
     * @return TransactionReportDto containing aggregated transaction information
     */
    @GetMapping("/transactions")
    public ResponseEntity<TransactionReportDto> generateTransactionReport(
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate,
            @RequestParam(required = false) String type) {
        
        logger.info("Received request to generate transaction report - startDate: {}, endDate: {}, type: {}",
                   startDate, endDate, type);

        TransactionReportDto report = reportService.generateTransactionReport(startDate, endDate, type);

        logger.info("Transaction report generated successfully with {} transactions",
                   report.getTotalTransactions());

        return ResponseEntity.ok(report);
    }

    /**
     * Generate card report
     * GET /api/reports/cards
     * 
     * @param status Optional filter by card status
     * @param expiringBefore Optional filter for cards expiring before this date (YYYY-MM-DD format)
     * @return CardReportDto containing aggregated card information
     */
    @GetMapping("/cards")
    public ResponseEntity<CardReportDto> generateCardReport(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String expiringBefore) {
        
        logger.info("Received request to generate card report - status: {}, expiringBefore: {}",
                   status, expiringBefore);

        CardReportDto report = reportService.generateCardReport(status, expiringBefore);

        logger.info("Card report generated successfully with {} cards",
                   report.getTotalCards());

        return ResponseEntity.ok(report);
    }
}
