import { defineStore } from 'pinia'
import { ref } from 'vue'

export const useTransactionStore = defineStore('transaction', () => {
  const currentTransaction = ref(null)
  const transactionList = ref([])
  const totalPages = ref(0)
  const currentPage = ref(0)
  const pageSize = ref(20)
  const totalElements = ref(0)
  const loading = ref(false)
  const error = ref(null)

  function setTransaction(transaction) {
    currentTransaction.value = transaction
  }

  function setTransactionList(transactions) {
    transactionList.value = transactions
  }

  function setPagination(pagination) {
    totalPages.value = pagination.totalPages || 0
    currentPage.value = pagination.currentPage || 0
    pageSize.value = pagination.pageSize || 20
    totalElements.value = pagination.totalElements || 0
  }

  function addTransaction(transaction) {
    transactionList.value.unshift(transaction)
  }

  function setLoading(isLoading) {
    loading.value = isLoading
  }

  function setError(errorMessage) {
    error.value = errorMessage
  }

  function clearTransaction() {
    currentTransaction.value = null
    error.value = null
  }

  function clearTransactionList() {
    transactionList.value = []
    totalPages.value = 0
    currentPage.value = 0
    totalElements.value = 0
  }

  function clearError() {
    error.value = null
  }

  return {
    currentTransaction,
    transactionList,
    totalPages,
    currentPage,
    pageSize,
    totalElements,
    loading,
    error,
    setTransaction,
    setTransactionList,
    setPagination,
    addTransaction,
    setLoading,
    setError,
    clearTransaction,
    clearTransactionList,
    clearError
  }
})
