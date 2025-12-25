import { defineStore } from 'pinia'
import { ref } from 'vue'

export const useCardStore = defineStore('card', () => {
  const currentCard = ref(null)
  const cardList = ref([])
  const loading = ref(false)
  const error = ref(null)

  function setCard(card) {
    currentCard.value = card
  }

  function setCardList(cards) {
    cardList.value = cards
  }

  function addCard(card) {
    cardList.value.push(card)
  }

  function updateCardInList(updatedCard) {
    const index = cardList.value.findIndex(c => c.cardNumber === updatedCard.cardNumber)
    if (index !== -1) {
      cardList.value[index] = updatedCard
    }
  }

  function removeCardFromList(cardNumber) {
    cardList.value = cardList.value.filter(c => c.cardNumber !== cardNumber)
  }

  function setLoading(isLoading) {
    loading.value = isLoading
  }

  function setError(errorMessage) {
    error.value = errorMessage
  }

  function clearCard() {
    currentCard.value = null
    error.value = null
  }

  function clearCardList() {
    cardList.value = []
  }

  function clearError() {
    error.value = null
  }

  return {
    currentCard,
    cardList,
    loading,
    error,
    setCard,
    setCardList,
    addCard,
    updateCardInList,
    removeCardFromList,
    setLoading,
    setError,
    clearCard,
    clearCardList,
    clearError
  }
})
