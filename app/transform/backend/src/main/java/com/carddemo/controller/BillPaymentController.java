package com.carddemo.controller;

import com.carddemo.dto.BillInfoDto;
import com.carddemo.dto.BillPaymentDto;
import com.carddemo.dto.TransactionDto;
import com.carddemo.service.BillPaymentService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller for bill payment operations.
 * Handles bill payment processing and bill information retrieval.
 */
@RestController
@RequestMapping("/api/bills")
@CrossOrigin(origins = "*")
public class BillPaymentController {
    
    private static final Logger logger = LoggerFactory.getLogger(BillPaymentController.class);
    
    private final BillPaymentService billPaymentService;
    
    public BillPaymentController(BillPaymentService billPaymentService) {
        this.billPaymentService = billPaymentService;
    }
    
    /**
     * Process a bill payment.
     * Creates a payment transaction and updates the account balance.
     * 
     * POST /api/bills/payment
     * 
     * @param billPayment the payment details
     * @return the created transaction
     */
    @PostMapping("/payment")
    public ResponseEntity<TransactionDto> processBillPayment(@RequestBody BillPaymentDto billPayment) {
        logger.info("Processing bill payment for account: {}", billPayment.getAccountId());
        
        TransactionDto transaction = billPaymentService.processBillPayment(billPayment);
        
        logger.info("Bill payment processed successfully for account: {}, transaction ID: {}", 
                   billPayment.getAccountId(), transaction.getTransactionId());
        
        return ResponseEntity.status(HttpStatus.CREATED).body(transaction);
    }
    
    /**
     * Get bill information for an account.
     * Returns current balance, minimum payment, and due date.
     * 
     * GET /api/bills/account/{accountId}
     * 
     * @param accountId the account ID
     * @return the bill information
     */
    @GetMapping("/account/{accountId}")
    public ResponseEntity<BillInfoDto> getBillInfo(@PathVariable Long accountId) {
        logger.info("Retrieving bill info for account: {}", accountId);
        
        BillInfoDto billInfo = billPaymentService.getBillInfo(accountId);
        
        logger.info("Bill info retrieved successfully for account: {}", accountId);
        
        return ResponseEntity.ok(billInfo);
    }
}
