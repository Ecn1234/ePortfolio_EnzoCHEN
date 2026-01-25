// Menu mobile
const hamburger = document.querySelector('.hamburger');
const navMenu = document.querySelector('.nav-menu');
const navLinks = document.querySelectorAll('.nav-link');

hamburger.addEventListener('click', () => {
    hamburger.classList.toggle('active');
    navMenu.classList.toggle('active');
});

navLinks.forEach(link => {
    link.addEventListener('click', () => {
        hamburger.classList.remove('active');
        navMenu.classList.remove('active');
        
        // Update active link
        navLinks.forEach(l => l.classList.remove('active'));
        link.classList.add('active');
    });
});

// Smooth scroll for anchor links
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
        e.preventDefault();
        
        const targetId = this.getAttribute('href');
        if (targetId === '#') return;
        
        const targetElement = document.querySelector(targetId);
        if (targetElement) {
            window.scrollTo({
                top: targetElement.offsetTop - 80,
                behavior: 'smooth'
            });
        }
    });
});

// Update active nav link on scroll
const sections = document.querySelectorAll('section');
const navHeight = 80;

window.addEventListener('scroll', () => {
    let current = '';
    
    sections.forEach(section => {
        const sectionTop = section.offsetTop - navHeight;
        const sectionHeight = section.clientHeight;
        
        if (scrollY >= sectionTop && scrollY < sectionTop + sectionHeight) {
            current = section.getAttribute('id');
        }
    });
    
    navLinks.forEach(link => {
        link.classList.remove('active');
        if (link.getAttribute('href') === `#${current}`) {
            link.classList.add('active');
        }
    });
});

// Back to top button
const backToTopBtn = document.getElementById('back-to-top');

window.addEventListener('scroll', () => {
    if (window.scrollY > 300) {
        backToTopBtn.classList.add('visible');
    } else {
        backToTopBtn.classList.remove('visible');
    }
});

backToTopBtn.addEventListener('click', () => {
    window.scrollTo({
        top: 0,
        behavior: 'smooth'
    });
});

// Filter projets
const filterButtons = document.querySelectorAll('.filter-btn');
const projetCards = document.querySelectorAll('.projet-card');

filterButtons.forEach(button => {
    button.addEventListener('click', () => {
        // Remove active class from all buttons
        filterButtons.forEach(btn => btn.classList.remove('active'));
        // Add active class to clicked button
        button.classList.add('active');
        
        const filterValue = button.getAttribute('data-filter');
        
        projetCards.forEach(card => {
            if (filterValue === 'all' || card.getAttribute('data-category') === filterValue) {
                card.style.display = 'flex';
                setTimeout(() => {
                    card.style.opacity = '1';
                    card.style.transform = 'translateY(0)';
                }, 10);
            } else {
                card.style.opacity = '0';
                card.style.transform = 'translateY(20px)';
                setTimeout(() => {
                    card.style.display = 'none';
                }, 300);
            }
        });
    });
});

// Modal pour les détails des projets
const modal = document.getElementById('projet-modal');
const modalClose = modal.querySelector('.modal-close');
const modalTitle = modal.querySelector('#modal-title');
const modalContent = modal.querySelector('#modal-content');
const detailButtons = document.querySelectorAll('.details-btn');

// Données des projets
const projetsData = {
    1: {
        title: "Analyse de profils clients - Cardio Good Fitness",
        description: "Analyse approfondie des profils de 180 clients de tapis de course pour identifier les segments de marché et optimiser les stratégies marketing.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Identifier les différents profils de clients pour personnaliser les offres commerciales et améliorer la rétention.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Analyse descriptive des variables sociodémographiques</li>
                    <li>Tests statistiques (ANOVA, tests d'indépendance)</li>
                    <li>Segmentation par clustering K-means</li>
                    <li>Analyse de variance pour identifier les différences significatives</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">R</span>
                    <span class="tag">ggplot2</span>
                    <span class="tag">dplyr</span>
                    <span class="tag">FactoMineR</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Identification de 4 segments de clients avec des comportements d'achat distincts. Recommandations stratégiques pour chaque segment.</p>
            </div>
        `
    },
    2: {
        title: "Analyse série temporelle - Pollution CO2",
        description: "Étude de la concentration en CO2 dans le métro parisien pour prévoir les pics de pollution.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Prévoir la concentration en CO2 dans la station Châtelet pour optimiser la ventilation.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Collecte de données horaires sur 6 mois</li>
                    <li>Analyse de la stationnarité (tests ADF)</li>
                    <li>Identification de modèle ARIMA/SARIMA</li>
                    <li>Validation croisée des prévisions</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">Python</span>
                    <span class="tag">Pandas</span>
                    <span class="tag">Statsmodels</span>
                    <span class="tag">Matplotlib</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Modèle SARIMA(1,1,1)(1,1,1,24) avec MAPE de 8.2%. Prévisions permettant d'anticiper les pics de pollution.</p>
            </div>
        `
    },
    3: {
        title: "Analyse de conformité RGPD",
        description: "Audit de conformité RGPD pour une entreprise de grande distribution.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Évaluer la conformité RGPD et rédiger une Analyse d'Impact sur la Protection des Données (AIPD).</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Cartographie des traitements de données</li>
                    <li>Analyse des risques pour les droits et libertés</li>
                    <li>Évaluation des mesures de sécurité existantes</li>
                    <li>Rédaction de l'AIPD conforme CNIL</li>
                </ul>
                
                <h3>Livrables</h3>
                <div class="tech-tags">
                    <span class="tag">AIPD</span>
                    <span class="tag">Cartographie</span>
                    <span class="tag">Analyse de risques</span>
                    <span class="tag">RGPD</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Identification de 3 traitements à risque nécessitant des mesures correctives. AIPD validée par le DPO.</p>
            </div>
        `
    },
    4: {
        title: "Système automatisé de pilotage SAS",
        description: "Développement d'un système d'automatisation des analyses statistiques avec interface Excel.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Automatiser les procédures SAS courantes via une interface utilisateur Excel.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Développement de macros SAS</li>
                    <li>Création d'interface Excel avec VBA</li>
                    <li>Automatisation du traitement de données</li>
                    <li>Génération automatique de rapports</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">SAS</span>
                    <span class="tag">VBA</span>
                    <span class="tag">Excel</span>
                    <span class="tag">Macros</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Réduction de 70% du temps de traitement. Interface intuitive pour les utilisateurs non techniques.</p>
            </div>
        `
    },
    5: {
        title: "Nettoyage et structuration de données",
        description: "Nettoyage de fichiers textes complexes sur des films pour création d'une base de données structurée.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Transformer des fichiers textes non structurés en base de données exploitable.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Parsing des fichiers texte avec expressions régulières</li>
                    <li>Nettoyage des données (valeurs manquantes, incohérences)</li>
                    <li>Structuration en tables relationnelles</li>
                    <li>Export en format CSV et SQL</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">Python</span>
                    <span class="tag">Regex</span>
                    <span class="tag">Pandas</span>
                    <span class="tag">CSV</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Base de données de 1500 films avec métadonnées structurées. Réduction de 95% des erreurs de données.</p>
            </div>
        `
    },
    6: {
        title: "Analyse du bonheur dans le monde",
        description: "Étude de l'influence du PIB et du COVID-19 sur le bonheur des pays.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Analyser les déterminants du bonheur et l'impact de la pandémie COVID-19.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Analyse descriptive des données World Happiness Report</li>
                    <li>Régression linéaire multiple</li>
                    <li>Comparaison pré/post COVID-19</li>
                    <li>Visualisation interactive des résultats</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">R</span>
                    <span class="tag">ggplot2</span>
                    <span class="tag">Shiny</span>
                    <span class="tag">Markdown</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Le PIB explique 62% de la variance du bonheur. Impact significatif du COVID-19 sur la santé mentale.</p>
            </div>
        `
    },
    7: {
        title: "Enquête pratiques culturelles étudiants",
        description: "Enquête sur l'impact du numérique sur les pratiques culturelles des étudiants.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Comprendre l'évolution des pratiques culturelles avec la digitalisation.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Conception du questionnaire (LimeSurvey)</li>
                    <li>Échantillonnage stratifié de 300 étudiants</li>
                    <li>Analyse statistique avec tests khi-deux</li>
                    <li>Base de données SQL pour le stockage</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">SQL</span>
                    <span class="tag">Excel</span>
                    <span class="tag">LimeSurvey</span>
                    <span class="tag">SPSS</span>
                </div>
                
                <h3>Résultats</h3>
                <p>82% des étudiants privilégient les contenus numériques. Corrélation forte entre usage numérique et diminution des sorties culturelles.</p>
            </div>
        `
    },
    8: {
        title: "Estimation par échantillonnage",
        description: "Estimation de la magnitude apparente moyenne d'étoiles par méthodes d'échantillonnage.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Estimer avec précision la magnitude moyenne d'une population d'étoiles à partir d'échantillons.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Échantillonnage aléatoire simple et stratifié</li>
                    <li>Calcul d'intervalles de confiance à 95%</li>
                    <li>Tests d'hypothèses sur la moyenne</li>
                    <li>Analyse de la puissance des tests</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">R</span>
                    <span class="tag">Statistiques</span>
                    <span class="tag">Échantillonnage</span>
                    <span class="tag">LaTeX</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Estimation de magnitude moyenne: 4.2 ± 0.15. Échantillonnage stratifié 30% plus efficace que simple.</p>
            </div>
        `
    },
    9: {
        title: "Modélisation de courbes de croissance",
        description: "Étude de la croissance staturale des enfants via régression polynomiale.",
        details: `
            <div class="projet-details">
                <h3>Objectif</h3>
                <p>Modéliser la croissance des enfants pour détecter d'éventuels retards de croissance.</p>
                
                <h3>Méthodologie</h3>
                <ul>
                    <li>Régression polynomiale (degré 2 à 5)</li>
                    <li>Validation croisée pour sélection du modèle</li>
                    <li>Analyse des résidus</li>
                    <li>Comparaison avec les courbes de référence OMS</li>
                </ul>
                
                <h3>Technologies utilisées</h3>
                <div class="tech-tags">
                    <span class="tag">Python</span>
                    <span class="tag">Scikit-learn</span>
                    <span class="tag">Matplotlib</span>
                    <span class="tag">NumPy</span>
                </div>
                
                <h3>Résultats</h3>
                <p>Modèle polynomial degré 3 optimal (R² = 0.94). Outil de détection précoce des retards de croissance.</p>
            </div>
        `
    }
};

// Ouvrir modal avec les détails du projet
detailButtons.forEach(button => {
    button.addEventListener('click', () => {
        const projetId = button.getAttribute('data-projet');
        const projet = projetsData[projetId];
        
        if (projet) {
            modalTitle.textContent = projet.title;
            modalContent.innerHTML = `
                <p class="projet-modal-description">${projet.description}</p>
                ${projet.details}
            `;
            modal.classList.add('active');
            document.body.style.overflow = 'hidden';
        }
    });
});

// Fermer modal
modalClose.addEventListener('click', () => {
    modal.classList.remove('active');
    document.body.style.overflow = 'auto';
});

// Fermer modal en cliquant en dehors
modal.addEventListener('click', (e) => {
    if (e.target === modal) {
        modal.classList.remove('active');
        document.body.style.overflow = 'auto';
    }
});

// Fermer modal avec ESC
document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && modal.classList.contains('active')) {
        modal.classList.remove('active');
        document.body.style.overflow = 'auto';
    }
});

// Form contact
const contactForm = document.getElementById('contact-form');
const formMessage = document.getElementById('form-message');

contactForm.addEventListener('submit', (e) => {
    e.preventDefault();
    
    // Récupération des valeurs
    const formData = {
        name: document.getElementById('name').value,
        email: document.getElementById('email').value,
        subject: document.getElementById('subject').value,
        message: document.getElementById('message').value
    };
    
    // Validation simple
    if (!formData.name || !formData.email || !formData.message) {
        showFormMessage('Veuillez remplir tous les champs obligatoires', 'error');
        return;
    }
    
    // Simulation d'envoi (remplacer par appel API réel)
    showFormMessage('Envoi en cours...', 'info');
    
    setTimeout(() => {
        showFormMessage('Message envoyé avec succès ! Je vous répondrai dans les plus brefs délais.', 'success');
        contactForm.reset();
        
        // Réinitialiser après 5 secondes
        setTimeout(() => {
            formMessage.style.display = 'none';
        }, 5000);
    }, 1500);
});

function showFormMessage(text, type) {
    formMessage.textContent = text;
    formMessage.className = 'form-message ' + type;
    formMessage.style.display = 'block';
}

// Animation des éléments au défilement
const observerOptions = {
    threshold: 0.1,
    rootMargin: '0px 0px -50px 0px'
};

const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
        if (entry.isIntersecting) {
            entry.target.classList.add('animated');
        }
    });
}, observerOptions);

// Observer les éléments à animer
document.querySelectorAll('.info-card, .stats-card, .timeline-content, .tool-category, .projet-card').forEach(el => {
    observer.observe(el);
});

// Animation des éléments de timeline au scroll
const timelineItems = document.querySelectorAll('.timeline-item');

const timelineObserver = new IntersectionObserver((entries) => {
    entries.forEach((entry, index) => {
        if (entry.isIntersecting) {
            setTimeout(() => {
                entry.target.style.opacity = '1';
                entry.target.style.transform = 'translateX(0)';
            }, index * 200);
        }
    });
}, {
    threshold: 0.2
});

// Initialisation des animations au chargement
window.addEventListener('DOMContentLoaded', () => {
    // Animation de la barre de progression des langues
    const langueLevels = document.querySelectorAll('.langue-level');
    langueLevels.forEach(level => {
        const width = level.style.width;
        level.style.width = '0';
        setTimeout(() => {
            level.style.width = width;
        }, 500);
    });
    
    // Initialisation des animations de la timeline
    timelineItems.forEach(item => {
        item.style.opacity = '0';
        item.style.transform = 'translateX(-20px)';
        item.style.transition = 'opacity 0.5s ease, transform 0.5s ease';
        timelineObserver.observe(item);
    });
    
    // Animation des chiffres (si ajouté plus tard)
    const counters = document.querySelectorAll('.counter');
    counters.forEach(counter => {
        const target = +counter.getAttribute('data-target');
        const increment = target / 100;
        let current = 0;
        
        const updateCounter = () => {
            if (current < target) {
                current += increment;
                counter.textContent = Math.ceil(current);
                setTimeout(updateCounter, 20);
            } else {
                counter.textContent = target;
            }
        };
        
        observer.observe(counter);
        counter.classList.add('counter');
    });
});

// Effet de parallaxe pour l'arrière-plan
window.addEventListener('scroll', () => {
    const scrolled = window.pageYOffset;
    const parallaxElements = document.querySelectorAll('.parallax');
    
    parallaxElements.forEach(element => {
        const speed = element.getAttribute('data-speed') || 0.5;
        element.style.transform = `translateY(${scrolled * speed}px)`;
    });
});