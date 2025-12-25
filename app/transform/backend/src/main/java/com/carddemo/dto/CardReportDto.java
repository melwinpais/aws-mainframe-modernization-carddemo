package com.carddemo.dto;

import java.util.List;

/**
 * DTO for Card Report
 * Contains aggregated card information for reporting purposes
 */
public class CardReportDto {
    private Integer totalCards;
    private Integer activeCards;
    private Integer inactiveCards;
    private Integer expiringCards;
    private List<CardSummaryDto> cards;

    public CardReportDto() {
    }

    public CardReportDto(Integer totalCards, Integer activeCards, Integer inactiveCards,
                        Integer expiringCards, List<CardSummaryDto> cards) {
        this.totalCards = totalCards;
        this.activeCards = activeCards;
        this.inactiveCards = inactiveCards;
        this.expiringCards = expiringCards;
        this.cards = cards;
    }

    // Getters and Setters
    public Integer getTotalCards() {
        return totalCards;
    }

    public void setTotalCards(Integer totalCards) {
        this.totalCards = totalCards;
    }

    public Integer getActiveCards() {
        return activeCards;
    }

    public void setActiveCards(Integer activeCards) {
        this.activeCards = activeCards;
    }

    public Integer getInactiveCards() {
        return inactiveCards;
    }

    public void setInactiveCards(Integer inactiveCards) {
        this.inactiveCards = inactiveCards;
    }

    public Integer getExpiringCards() {
        return expiringCards;
    }

    public void setExpiringCards(Integer expiringCards) {
        this.expiringCards = expiringCards;
    }

    public List<CardSummaryDto> getCards() {
        return cards;
    }

    public void setCards(List<CardSummaryDto> cards) {
        this.cards = cards;
    }

    /**
     * Inner DTO for individual card summary in report
     */
    public static class CardSummaryDto {
        private Long cardNumber;
        private Long accountId;
        private Long customerId;
        private String cardStatus;
        private String expirationDate;
        private String issueDate;

        public CardSummaryDto() {
        }

        public CardSummaryDto(Long cardNumber, Long accountId, Long customerId,
                             String cardStatus, String expirationDate, String issueDate) {
            this.cardNumber = cardNumber;
            this.accountId = accountId;
            this.customerId = customerId;
            this.cardStatus = cardStatus;
            this.expirationDate = expirationDate;
            this.issueDate = issueDate;
        }

        // Getters and Setters
        public Long getCardNumber() {
            return cardNumber;
        }

        public void setCardNumber(Long cardNumber) {
            this.cardNumber = cardNumber;
        }

        public Long getAccountId() {
            return accountId;
        }

        public void setAccountId(Long accountId) {
            this.accountId = accountId;
        }

        public Long getCustomerId() {
            return customerId;
        }

        public void setCustomerId(Long customerId) {
            this.customerId = customerId;
        }

        public String getCardStatus() {
            return cardStatus;
        }

        public void setCardStatus(String cardStatus) {
            this.cardStatus = cardStatus;
        }

        public String getExpirationDate() {
            return expirationDate;
        }

        public void setExpirationDate(String expirationDate) {
            this.expirationDate = expirationDate;
        }

        public String getIssueDate() {
            return issueDate;
        }

        public void setIssueDate(String issueDate) {
            this.issueDate = issueDate;
        }
    }
}
