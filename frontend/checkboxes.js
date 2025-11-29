const API_URL = 'http://localhost:8080';

let checkboxes = [];
let eventSource = null;

function getCheckboxKey(id) {
    return `checkbox-${id}`;
}

function createCheckboxElement(checkbox) {
    const item = document.createElement('div');
    item.className = 'checkbox-item';
    
    const checkboxInput = document.createElement('input');
    checkboxInput.type = 'checkbox';
    checkboxInput.checked = checkbox.checked;
    checkboxInput.id = getCheckboxKey(checkbox.id);
    checkboxInput.addEventListener('change', (e) => {
        updateCheckbox(checkbox.id, e.target.checked);
    });
    
    const label = document.createElement('label');
    label.htmlFor = getCheckboxKey(checkbox.id);
    label.textContent = `#${checkbox.id}`;
    
    item.appendChild(checkboxInput);
    item.appendChild(label);
    return item;
}

function updateCheckboxElement(id, checked) {
    const checkboxElement = document.getElementById(getCheckboxKey(id));
    if (checkboxElement && checkboxElement.checked !== checked) {
        checkboxElement.checked = checked;
    }
}

function syncCheckboxes(newCheckboxes) {
    const container = document.getElementById('checkboxes-container');
    
    const existingMap = new Map();
    checkboxes.forEach(cb => existingMap.set(cb.id, cb));
    
    newCheckboxes.forEach(checkbox => {
        const existing = existingMap.get(checkbox.id);
        if (!existing) {
            const element = createCheckboxElement(checkbox);
            container.appendChild(element);
        } else if (existing.checked !== checkbox.checked) {
            updateCheckboxElement(checkbox.id, checkbox.checked);
        }
    });
    
    checkboxes = newCheckboxes;
    updateStats();
}

async function fetchCheckboxes() {
    try {
        const response = await fetch(`${API_URL}/checkboxes`);
        if (!response.ok) throw new Error(`Erro HTTP: ${response.status}`);
        
        const data = await response.json();
        if (data.checkboxes && Array.isArray(data.checkboxes)) {
            syncCheckboxes(data.checkboxes);
        }
    } catch (error) {
        console.error('Erro ao buscar checkboxes:', error);
    }
}

async function updateCheckbox(id, checked) {
    try {
        const response = await fetch(`${API_URL}/checkboxes/${id}`, {
            method: 'PATCH',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ checked: checked })
        });
        
        if (!response.ok) {
            throw new Error(`Erro HTTP: ${response.status}`);
        }
        
        const updated = await response.json();
        const index = checkboxes.findIndex(cb => cb.id === id);
        if (index !== -1) {
            checkboxes[index] = updated;
            updateCheckboxElement(id, updated.checked);
            updateStats();
        }
    } catch (error) {
        console.error('Erro ao atualizar checkbox:', error);
        const checkboxElement = document.getElementById(getCheckboxKey(id));
        if (checkboxElement) {
            checkboxElement.checked = !checked;
        }
    }
}

function updateStats() {
    const checkedCount = checkboxes.filter(cb => cb.checked).length;
    const totalCount = checkboxes.length;
    const checkedElement = document.getElementById('checked-count');
    const totalElement = document.getElementById('total-count');
    
    if (checkedElement) checkedElement.textContent = checkedCount;
    if (totalElement) totalElement.textContent = totalCount;
}

function startSSE() {
    if (eventSource) {
        eventSource.close();
    }
    
    eventSource = new EventSource(`${API_URL}/events`);
    
    eventSource.onopen = () => {
        console.log('SSE connection opened');
    };
    
    eventSource.onmessage = (event) => {
        if (event.data === 'connected') {
            console.log('SSE connected');
            return;
        }
        
        try {
            const updated = JSON.parse(event.data);
            const index = checkboxes.findIndex(cb => cb.id === updated.id);
            if (index !== -1) {
                checkboxes[index] = updated;
                updateCheckboxElement(updated.id, updated.checked);
                updateStats();
            } else {
                fetchCheckboxes();
            }
        } catch (error) {
            console.error('Erro ao processar evento SSE:', error);
        }
    };
    
    eventSource.onerror = (error) => {
        console.error('SSE error:', error);
        eventSource.close();
        setTimeout(() => {
            startSSE();
        }, 3000);
    };
}

function stopSSE() {
    if (eventSource) {
        eventSource.close();
        eventSource = null;
    }
}

window.addEventListener('load', () => {
    fetchCheckboxes();
    startSSE();
});

window.addEventListener('beforeunload', () => {
    stopSSE();
});

window.addEventListener('visibilitychange', () => {
    if (document.hidden) {
        stopSSE();
    } else {
        fetchCheckboxes();
        startSSE();
    }
});
