import { defineStore } from 'pinia'
import { ref } from 'vue'

export const useAccountStore = defineStore('account', () => {
  const currentAccount = ref(null)
  const currentCustomer = ref(null)
  const currentCards = ref([])
  const searchResults = ref([])
  const loading = ref(false)
  const error = ref(null)

  function setAccount(account) {
    currentAccount.value = account
  }

  function setCustomer(customer) {
    currentCustomer.value = customer
  }

  function setCards(cards) {
    currentCards.value = cards
  }

  function setSearchResults(results) {
    searchResults.value = results
  }

  function setLoading(isLoading) {
    loading.value = isLoading
  }

  function setError(errorMessage) {
    error.value = errorMessage
  }

  function clearAccount() {
    currentAccount.value = null
    currentCustomer.value = null
    currentCards.value = []
    error.value = null
  }

  function clearSearchResults() {
    searchResults.value = []
  }

  function clearError() {
    error.value = null
  }

  return {
    currentAccount,
    currentCustomer,
    currentCards,
    searchResults,
    loading,
    error,
    setAccount,
    setCustomer,
    setCards,
    setSearchResults,
    setLoading,
    setError,
    clearAccount,
    clearSearchResults,
    clearError
  }
})
