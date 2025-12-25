package com.carddemo.service;

import com.carddemo.dto.BillInfoDto;
import com.carddemo.dto.BillPaymentDto;
import com.carddemo.dto.TransactionDto;

public interface BillPaymentService {
    /**
     * Process a bill payment for an account
     * Creates a payment transaction and updates the account balance
     * 
     * @param billPayment the payment details
     * @return the created transaction
     */
    TransactionDto processBillPayment(BillPaymentDto billPayment);

    /**
     * Get bill information for an account
     * Returns current balance, minimum payment, and due date
     * 
     * @param accountId the account ID
     * @return the bill information
     */
    BillInfoDto getBillInfo(Long accountId);
}
